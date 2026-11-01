// Reports Pi's events to Switchboard in Emacs. Switchboard loads it into
// each Pi session it starts with `pi -e`; anything else is left alone.
//
// It sends the events Switchboard already handles from Claude Code, under
// the same names, through switchboard-hook.sh:
//
//   SessionStart       a session starts, resumes, or replaces another (/new)
//   UserPromptSubmit   a run starts, from your prompt or a subagent's wake
//   PreToolUse         a tool starts (or a prompt it asked you was answered)
//   PostToolUse        a tool finished
//   PermissionRequest  Pi waits on a prompt to you, mid-run
//   Stop / StopFailure the run settled, or ended on an error
//   Aborted            you interrupted it
//   SessionInfo        the session's name changed
//   SubagentStart/Stop pi-subagents' children: foreground ones from the
//                      subagent tool's progress, background ones from its
//                      async-started and async-complete events
//   PreToolUse         with agent_id: the tool a child runs; a background
//                      child's comes from pi-subagents' status, read
//                      when it says its jobs changed
//
// Payloads carry only what Switchboard reads, and go out one at a time in
// the order Pi raised them.

import { spawn } from "node:child_process";
import { randomUUID } from "node:crypto";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const hook = join(dirname(fileURLToPath(import.meta.url)), "switchboard-hook.sh");

let queue: Promise<void> = Promise.resolve();

function send(event: string, payload: Record<string, unknown>) {
  const json = JSON.stringify(payload);
  queue = queue.then(
    () =>
      new Promise<void>((resolve) => {
        const p = spawn("/bin/sh", [hook, "pi", event], { stdio: ["pipe", "ignore", "ignore"] });
        p.on("error", () => resolve());
        p.on("close", () => resolve());
        p.stdin.on("error", () => {});
        p.stdin.end(json);
      }),
  );
}

// A task's first line, short: Switchboard shows it on one line.
// pi-subagents redacts the tasks it reports, so prefer the tool call's.
function summary(text: unknown): string | undefined {
  if (typeof text !== "string" || /^\[.*redacted\]$/.test(text.trim())) return undefined;
  const line = text.trim().split("\n")[0];
  return line.length > 120 ? line.slice(0, 119) + "…" : line || undefined;
}

// What a tool call is working on, as Claude Code's tool_input names it.
function toolInput(args: any): Record<string, unknown> | undefined {
  if (!args || typeof args !== "object") return undefined;
  const pick = (v: unknown) => (typeof v === "string" ? v : undefined);
  return { command: pick(args.command), file_path: pick(args.path ?? args.file_path), url: pick(args.url) };
}

export default function (pi: any) {
  if (!process.env.SWITCHBOARD_ID) return;

  let active = false; // this runtime reports: the session isn't a child's
  let sessionId: string | undefined;
  let sessionFile: string | undefined;
  let cwd: string | undefined;
  let running = false; // between a run starting and settling
  let outcome: string | undefined;
  let error: string | undefined;
  let tokens: number | undefined;
  let tool: { name: string; args: any } | undefined; // the main agent's, in flight

  const base = () => ({ session_id: sessionId, cwd });
  const usage = (ctx: any) => {
    const n = ctx?.getContextUsage?.()?.tokens;
    if (typeof n === "number") tokens = n;
    return tokens;
  };

  // Foreground children, by "runId:index", and the tool calls that started
  // them; background ones by run id.
  type Child = { call: string; agent: string; tool?: string; done: boolean; background?: boolean };
  const children = new Map<string, Child>();
  const calls = new Map<string, any>(); // subagent tool call id to its arguments
  // Child INDEX's task in a subagent call: one task, or one of parallel ones.
  const taskOf = (callId: string | undefined, index = 0) => {
    const args = callId ? calls.get(callId) : undefined;
    return args?.task ?? args?.tasks?.[index]?.task ?? args?.chain?.[index]?.task;
  };

  const startChild = (id: string, call: string | undefined, agent: string, tasks: unknown[], background?: boolean) => {
    if (children.has(id)) return children.get(id)!;
    const c: Child = { call: call ?? "", agent, done: false, background };
    children.set(id, c);
    const description = tasks.map(summary).find((t) => t);
    send("SubagentStart", { ...base(), agent_id: id, agent_type: agent, description, background });
    return c;
  };
  const stopChild = (id: string) => {
    const c = children.get(id);
    if (!c || c.done) return;
    c.done = true;
    send("SubagentStop", { ...base(), agent_id: id });
  };

  const followProgress = (callId: string, details: any) => {
    if (!details || details.asyncId || details.background) return; // a background launch
    for (const row of details.progress ?? []) {
      if (!row || row.status === "pending") continue;
      const id = `${details.runId ?? callId}:${row.index}`;
      const c = startChild(id, callId, row.agent ?? "agent", [taskOf(callId, row.index), row.task]);
      if (row.currentTool && row.currentTool !== c.tool) {
        c.tool = row.currentTool;
        send("PreToolUse", { ...base(), agent_id: id, agent_type: row.agent, tool_name: row.currentTool });
      }
      if (row.status !== "running") stopChild(id);
    }
  };

  pi.on("session_start", (e: any, ctx: any) => {
    const sm = ctx.sessionManager;
    // pi-subagents records a child's parent in its header; Pi's own /fork
    // does too, but that one is yours.
    active = !(sm.getHeader?.()?.parentSession && e.reason !== "fork");
    if (!active) return;
    sessionId = sm.getSessionId();
    sessionFile = sm.getSessionFile?.();
    cwd = ctx.cwd;
    send("SessionStart", { ...base(), source: e.reason, title: sm.getSessionName?.(), tokens: usage(ctx) });
  });

  pi.on("session_info_changed", (e: any) => {
    if (active) send("SessionInfo", { ...base(), title: e.name });
  });

  pi.on("before_agent_start", (e: any, ctx: any) => {
    if (!active) return;
    running = true;
    outcome = error = undefined;
    send("UserPromptSubmit", {
      ...base(),
      ...(e.messageType === "user" ? { prompt: e.prompt } : {}),
      tokens: usage(ctx),
    });
  });

  pi.on("tool_execution_start", (e: any) => {
    if (!active) return;
    tool = { name: e.toolName, args: e.args };
    if (e.toolName === "subagent") calls.set(e.toolCallId, e.args);
    send("PreToolUse", { ...base(), tool_name: e.toolName });
  });

  pi.on("tool_execution_update", (e: any) => {
    if (active && e.toolName === "subagent") followProgress(e.toolCallId, e.partialResult?.details);
  });

  pi.on("tool_execution_end", (e: any) => {
    if (!active) return;
    if (e.toolName === "subagent") {
      followProgress(e.toolCallId, e.result?.details);
      // A foreground call returns once its children are done; one that
      // launched a background run returns right away.
      for (const [id, c] of children) if (c.call === e.toolCallId && !c.background) stopChild(id);
      calls.delete(e.toolCallId);
    }
    tool = undefined;
    send("PostToolUse", { ...base(), tool_name: e.toolName });
  });

  // Pi has no permission prompts of its own; extensions ask through these.
  pi.on("ui_prompt_start", (e: any) => {
    if (!active || !running) return;
    send("PermissionRequest", {
      ...base(),
      tool_name: tool?.name ?? e.title ?? e.kind,
      tool_input: tool ? toolInput(tool.args) : undefined,
    });
  });

  pi.on("ui_prompt_end", () => {
    if (active && running) send("PreToolUse", { ...base(), tool_name: tool?.name ?? "prompt" });
  });

  pi.on("agent_end", (e: any) => {
    if (!active) return;
    const last = [...(e.messages ?? [])].reverse().find((m: any) => m?.role === "assistant");
    if (last?.stopReason === "error") error = last.errorMessage;
  });

  pi.on("agent_before_settle", (e: any) => {
    if (active) outcome = e.outcome;
  });

  pi.on("agent_settled", (e: any, ctx: any) => {
    if (!active) return;
    running = false;
    tool = undefined;
    if (e.aborted || outcome === "aborted") send("Aborted", base());
    else if (outcome === "error" || error) send("StopFailure", { ...base(), error: error ?? "error" });
    else send("Stop", { ...base(), tokens: usage(ctx) });
  });

  // pi-subagents' background runs; its events carry no context.
  pi.events?.on?.("subagent:async-started", (d: any) => {
    // Its sessionId is the session's file, though older versions may send the id.
    if (!active || !d?.id || (d.sessionId && d.sessionId !== sessionFile && d.sessionId !== sessionId)) return;
    const agent = d.agent ?? d.agents?.[0] ?? "agent";
    startChild(d.id, d.toolCallId, agent, [taskOf(d.toolCallId), d.chain?.join(" → ")], true);
  });

  pi.events?.on?.("subagent:async-complete", (d: any) => {
    if (active) stopChild(d?.runId ?? d?.id);
  });

  // A background child's tool: pi-subagents only hints that its jobs
  // changed, up to once a second per run while one works, so read its
  // status at most every 250 ms, and once more if it changed meanwhile.
  const rpc = (method: string, params: object) =>
    new Promise<any>((resolve) => {
      const requestId = randomUUID();
      const done = (reply?: any) => {
        clearTimeout(timeout);
        off?.();
        resolve(reply);
      };
      const off = pi.events.on(`subagents:rpc:v1:reply:${requestId}`, done);
      const timeout = setTimeout(done, 5000);
      pi.events.emit("subagents:rpc:v1:request", { version: 1, requestId, method, params });
    });

  // A run's tool, or else the first one its steps (chain, parallel) run.
  const toolOf = (run: any): string | undefined =>
    run?.activity?.currentTool ?? (run?.children ?? []).map(toolOf).find((t: unknown) => t);

  const readTools = async () => {
    const reply = await rpc("status", {});
    for (const run of (reply?.success && reply.data?.asyncSnapshot?.runs) || []) {
      const c = children.get(run?.id);
      const tool = c && !c.done && c.background ? toolOf(run) : undefined;
      if (tool && tool !== c!.tool) {
        c!.tool = tool;
        send("PreToolUse", { ...base(), agent_id: run.id, agent_type: c!.agent, tool_name: tool });
      }
    }
  };

  let timer: ReturnType<typeof setTimeout> | undefined;
  let reading = false;
  let again = false;
  const changed = () => {
    if (!active || timer) return;
    if (![...children.values()].some((c) => c.background && !c.done)) return;
    timer = setTimeout(async () => {
      timer = undefined;
      if (reading) return void (again = true);
      reading = true;
      try {
        await readTools();
      } catch {}
      reading = false;
      if (again) {
        again = false;
        changed();
      }
    }, 250);
  };
  pi.events?.on?.("subagent:jobs-changed", changed);
}
