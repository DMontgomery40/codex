# Agent REPL Power Guide

A research-driven field guide for high-leverage workflows with coding agents and REPL-heavy loops, focused on:

- Codex CLI
- Claude Code
- OpenCode
- Qwen Code
- External REPL + terminal orchestration (tmux, zellij, shell loops, editor terminals)

Last updated: March 3, 2026.

## Scope and filtering

This guide intentionally skips beginner tips. It emphasizes:

- Tricks that compress iteration time
- Reliability loops (test, lint, CI, rollback)
- Multi-agent patterns
- External REPL command-center patterns
- Tactics repeatedly mentioned in GitHub discussions and X chatter

## Real REPL hacks from power users (actual field patterns)

This section is intentionally concrete and source-backed. No basics.

### 1) Codex REPL/operator tricks

1. Use `streamable_shell = true` for interactive CLI installers/wizards (for example `npx create-next-app`) where the agent can send keystrokes to running shells.
2. Turn on `js_repl` and use it as a persistent command bus with top-level `await`; state survives across calls.
3. If you want all direct tool calls to route through REPL orchestration, enable `js_repl_tools_only = true` and call tools from JS with `await codex.tool(...)`.
4. Use per-cell timeout pragmas in REPL (`// codex-js-repl: timeout_ms=15000`) and use `js_repl_reset` aggressively to recover from state poison.
5. Preload custom/private Node modules for REPL experiments via `CODEX_JS_REPL_NODE_MODULE_DIRS` or `js_repl_node_module_dirs`, and use dynamic import (`await import(...)`) instead of static top-level `import`.
6. Create role launchers in shell (`codex-swift`, `codex-bug`, etc.) that inject different instructions; this pattern comes directly from openai/codex discussion usage.
7. Keep reusable prompt macros under `~/.codex/prompts` and trigger them as slash-command inserts inside interactive sessions.

### 2) Claude Code REPL/operator tricks

1. Start long-running dev servers in background (`Ctrl+B`) and have Claude run integration tests against the live server loop.
2. For external orchestration, run print mode with event streaming: `claude -p --output-format stream-json --include-partial-messages`.
3. Branch alternative solutions from same context without collisions: `claude --resume <session> --fork-session`.
4. Gate risky tool usage with hooks using MCP regex matchers (for example `mcp__.*__write.*`) to block or warn before side effects.
5. Use `--agents '<json>'` for temporary per-run specialist agents instead of permanently editing agent config files.

### 3) OpenCode and Qwen REPL/operator tricks

1. Use `opencode run --attach http://localhost:4096 ...` against a long-lived `opencode web`/`serve` backend to avoid MCP cold boots each run.
2. Use OpenCode command templates with `$ARGUMENTS`, positional args, shell interpolation, and `subtask: true` to keep your main context clean.
3. In OpenCode permissions, exploit ordered rules with “last matching rule wins” and use `external_directory` to safely allow operations outside cwd.
4. In Qwen, cycle runtime approval posture during a live session with `Shift+Tab` (Plan, Auto-Edit, YOLO, etc.) instead of restarting.
5. Enable Qwen checkpointing and use `/restore` to hard-rewind files and conversation state after bad tool runs.
6. For script pipelines, use Qwen `--output-format stream-json --include-partial-messages` plus `--continue`/`--resume` for resumable automation loops.

### 4) External REPL stacks people are actively using

1. `tmux-mcp`: expose tmux sessions to agents so they can inspect panes, tail logs, and interact with live terminal workflows.
2. `codex-cli-farm`: run many Codex/Claude panes with durable logs, then `codex-save` and `codex-restore -a` to snapshot/rehydrate your whole farm.
3. `xlaude`: map each git worktree to an agent session and auto-append `codex resume <session-id>` when worktree `cwd` matches.
4. `claude-code-commands`: issue-driven parallel implementation flows (`...run-parallel`) that open tmux panes for competing solutions.
5. `ccmanager`: multi-agent session manager with live busy/waiting/idle states and optional cross-worktree session-data copy.

### Source links for this section

- https://x.com/nummanali/status/1992321623597121710
- https://x.com/iannuttall/status/2022016174259941867
- https://github.com/openai/codex/discussions/7296
- https://raw.githubusercontent.com/openai/codex/main/docs/js_repl.md
- https://x.com/claude_code/status/1955210320244326460
- https://code.claude.com/docs/en/cli-reference.md
- https://code.claude.com/docs/en/hooks.md
- https://raw.githubusercontent.com/anomalyco/opencode/dev/packages/web/src/content/docs/commands.mdx
- https://raw.githubusercontent.com/anomalyco/opencode/dev/packages/web/src/content/docs/cli.mdx
- https://raw.githubusercontent.com/anomalyco/opencode/dev/packages/web/src/content/docs/permissions.mdx
- https://raw.githubusercontent.com/QwenLM/qwen-code-docs/main/website/content/en/users/features/approval-mode.md
- https://raw.githubusercontent.com/QwenLM/qwen-code-docs/main/website/content/en/users/features/checkpointing.md
- https://raw.githubusercontent.com/QwenLM/qwen-code-docs/main/website/content/en/users/features/headless.md
- https://x.com/camsoft2000/status/1964045997287596428
- https://raw.githubusercontent.com/waskosky/codex-cli-farm/main/README.md
- https://raw.githubusercontent.com/Xuanwo/xlaude/main/README.md
- https://raw.githubusercontent.com/o-gi/claude-code-commands/main/README.md
- https://raw.githubusercontent.com/kbwo/ccmanager/main/README.md

## 0) 10-minute setup for power users

1. Keep **one control terminal** and **one execution terminal** minimum.
2. Run each major task in its own **git worktree**.
3. Add project-local agent instructions (`AGENTS.md`, `.claude/`, `.qwen/commands`, `.opencode/commands`) before prompting.
4. Set up one **fast test command** and one **full gate command**.
5. Use JSON-stream/headless output modes for automation workflows.

## 1) Fast patterns that actually compound

### 1.1 Prompt contract > giant prompt

Use short, durable contracts in files, not long one-off prompts.

- Codex: `AGENTS.md` + `AGENTS.override.md` precedence lets you scope behavior by folder depth.
- Claude Code: settings + hooks + slash commands create repeatable guardrails.
- OpenCode: custom commands with args and `subtask: true` let you encode reusable playbooks.
- Qwen Code: command packs under `~/.qwen/commands` and `.qwen/commands` reduce prompt boilerplate.

### 1.2 Branch/worktree isolation for every agentic attempt

Stop sharing one dirty branch across experiments.

- Use one worktree per subtask/variant.
- Keep test outputs and logs scoped per worktree.
- Recombine with stacked PRs or selective cherry-picks.

### 1.3 Two-speed validation loop

Always have:

- `fast loop` (seconds): related tests/lint on changed files
- `hard gate` (minutes): full test/lint/build and CI parity checks

This keeps the REPL fast but trustworthy.

### 1.4 External REPL control plane

Internal tool REPLs are useful, but power users run a second control layer outside:

- tmux or zellij for pane/session orchestration
- watchers (`watchexec`, `entr`) for auto-rerun loops
- editor terminal jobs for inline smoke-test pipelines

## 2) Codex power moves (advanced)

### 2.1 Use profiles aggressively

Create task-specific profiles (speed coding, refactor, audit, docs) and switch with:

```bash
codex --profile <name>
```

### 2.2 Prefer one-off config overrides for experiments

Instead of editing config files repeatedly:

```bash
codex -c model_reasoning_effort="high"
codex -c approval_policy="never"
```

### 2.3 Keep repo rules local and durable

GitHub discussion users repeatedly report better reliability when they:

- keep personal defaults global
- keep repo-specific constraints in `AGENTS.md`

### 2.4 Use noninteractive mode for reliable pipelines

For scripts and CI:

```bash
codex exec --json "<task>"
codex exec --output-schema schema.json "<task>"
codex exec resume --last
```

### 2.5 Fail fast when critical MCP servers are missing

Mark essential MCP servers as required so runs fail loudly instead of silently degrading.

### 2.6 Multi-agent roles with separate config files

Use role-specific configs (reviewer, implementer, docs, debugger) to enforce behavior boundaries.

### 2.7 Community-proven quality trick

From Codex discussions: ask the model to clarify ambiguity before coding, and encode that behavior in `AGENTS.md`.

## 3) Claude Code power moves (advanced)

### 3.1 Use settings scopes as policy layers

Use managed/user/project/local scopes to separate:

- org policy
- team defaults
- local machine preferences

### 3.2 Session branching for parallel solution paths

```bash
claude --resume
claude --fork-session
```

This is one of the cleanest ways to compare alternate implementations without context collision.

### 3.3 Stream JSON output for machine loops

For scripting/automation:

```bash
claude -p --output-format stream-json --include-partial-messages "<task>"
```

### 3.4 Use hooks for hard guardrails

High-leverage hook uses:

- pre-edit policy checks
- post-edit lint/test hooks
- MCP tool-specific gating with `mcp__<server>__<tool>` patterns

### 3.5 X chatter pattern: worktree + subagents

Multiple power users describe running separate Claude sessions in isolated worktrees, then merging best diffs.

### 3.6 Keep slash commands as playbook entry points

Instead of “prompting from scratch,” maintain reusable slash commands for recurring workflows.

## 4) OpenCode power moves (advanced)

### 4.1 Treat permissions as code

OpenCode permission rules are high leverage because rule order matters (“last matching rule wins”).

### 4.2 External directory allowlists

Grant controlled read/edit outside workspace without opening everything.

### 4.3 Command system as reusable automation

Use custom commands with:

- `$ARGUMENTS`
- positional args
- `!shell` execution
- `@file` references
- `subtask: true` for context-isolated delegation

### 4.4 Remote backend + local attach

```bash
opencode web
opencode attach
```

Useful for remote devboxes and long-lived environments.

### 4.5 Git-backed undo/redo for fearless iteration

Power users leverage `/undo` and `/redo` as fast rollback primitives during aggressive edits.

## 5) Qwen Code power moves (advanced)

### 5.1 Permission mode cycling during a session

Switch among Default / Auto-Edit / Plan / YOLO quickly (Shift+Tab) instead of restarting with new configs.

### 5.2 Shadow checkpoint repo + restore

Checkpointing creates restore points before edits. Use this before risky refactors.

### 5.3 Command packs for reusable runbooks

Keep personal commands in `~/.qwen/commands`, project-specific in `.qwen/commands`.

### 5.4 Headless stream mode for pipelines

```bash
qwen --output-format stream-json --continue
qwen --output-format stream-json --resume
```

### 5.5 MCP exposure control

Use include/exclude tool lists per server and global allowed/excluded lists.

### 5.6 Sandbox priority awareness

`GEMINI_SANDBOX` can override expected behavior; be explicit when debugging tool execution context.

## 6) External REPL and CLI command-center patterns

This is where many 10x workflows are actually won.

### 6.1 tmux: practical elite setup

Use tmux as an agent command bus:

- `pipe-pane -o` for persistent logs
- `wait-for` channels as semaphores
- `remain-on-exit` + `respawn-pane` for failed loops
- `synchronize-panes` for fan-out commands
- control mode (`tmux -C`) for programmatic orchestration

Example skeleton:

```bash
tmux new-session -d -s agents
tmux split-window -h
tmux split-window -v
tmux setw synchronize-panes off

tmux send-keys -t agents:0.0 'codex' C-m
tmux send-keys -t agents:0.1 'claude' C-m
tmux send-keys -t agents:0.2 'opencode' C-m
```

### 6.2 zellij: layout-as-code workflows

Use checked-in layouts to make your environment reproducible across machines.

### 6.3 Watcher loops

Use file-triggered loops for immediate feedback:

```bash
watchexec -r -e ts,tsx,js -- npm test -- --runInBand
git ls-files '*.py' | entr -c pytest -q
```

### 6.4 Editor-terminal loops

- Neovim `:terminal` + job control for async loops.
- VS Code background tasks + problem matchers for persistent test/watch runners.

### 6.5 “Outside-REPL memory” pattern

Keep an external `scratchpad.md`/`ops.log` that captures:

- accepted design decisions
- commands run
- failure signatures
- next actions

This prevents context resets from killing momentum.

## 7) Multi-agent orchestration tactics that work

### 7.1 Role split

Use at least three roles:

- Planner (scope + checklist)
- Implementer (edits)
- Verifier (tests + regression + edge cases)

### 7.2 Communication protocol

Enforce short machine-checkable handoffs:

- `done`: what changed
- `proof`: tests/logs/screenshots
- `risk`: what might still break
- `next`: concrete follow-up command

### 7.3 Batch-size discipline

Keep each delegated task small enough to verify in one pass. Large fuzzy asks produce impressive but fragile output.

### 7.4 Worktree-per-role

Do not let planner and implementer write in the same worktree.

### 7.5 Anti-flake verifier loop

Before merge:

1. run targeted tests twice
2. run full suite once
3. run lint/format checks
4. compare diff size against task scope

## 8) X chatter highlights (high-signal themes)

Cross-thread themes that show up repeatedly:

- Use worktrees for parallel attempts instead of one evolving chat.
- Prefer short iterative prompts with strict success criteria.
- Let agents run dev servers/background jobs and validate with integration tests.
- Persist conventions/rules in files, not memory.
- Use tool modes dynamically (planning vs execution) per phase.
- External terminal multiplexing is a force multiplier for long tasks.

## 9) GitHub discussion highlights (high-signal themes)

### 9.1 Instruction layering works

Codex discussion participants report cleaner outcomes when global personal defaults are separated from repo-local `AGENTS.md`.

### 9.2 Clarification-first behavior reduces wasted edits

Several users describe adding explicit “ask first on ambiguity” rules in `AGENTS.md` to prevent runaway implementation.

### 9.3 WSL and filesystem placement matter for reliability

In the WSL testing thread, users report better behavior when repos live on native WSL filesystem paths (`~/...`) vs mounted Windows drives (`/mnt/...`) for some workflows.

## 10) 1337 command snippets you can drop in today

### 10.1 Fast branch + worktree

```bash
git fetch origin
git worktree add -b feat/agent-exp-1 ../wt-agent-exp-1 origin/main
git worktree add -b feat/agent-exp-2 ../wt-agent-exp-2 origin/main
```

### 10.2 CI check watch loop

```bash
gh pr checks --required --watch --fail-fast
gh run watch --exit-status
```

### 10.3 Agent-safe push

```bash
git push --force-with-lease origin HEAD
```

### 10.4 Pane log capture

```bash
tmux pipe-pane -o 'cat >> .logs/pane-#{pane_id}.log'
```

### 10.5 Changed-files test loop (Jest)

```bash
jest -o --watch
jest --findRelatedTests $(git diff --name-only origin/main...HEAD)
```

### 10.6 Rust narrow-first loop

```bash
cargo test -p <crate> <test_name> -- --nocapture
```

### 10.7 Codex schema-constrained automation

```bash
codex exec --output-schema ./schema.json "summarize breaking changes"
```

### 10.8 Claude stream-json pipeline

```bash
claude -p --output-format stream-json --include-partial-messages "review this diff for risks"
```

### 10.9 OpenCode attach to remote backend

```bash
opencode web
opencode attach
```

### 10.10 Qwen checkpoint + restore habit

Use checkpointing before large refactors and restore immediately when regressions appear.

## 11) Mistakes even advanced users keep making

1. Delegating without test gates.
2. Running multiple agents in one dirty branch.
3. Treating model memory as a source of truth instead of policy files.
4. Using massive prompts instead of repeatable command files/hook rules.
5. Skipping external logging when running long agent sessions.
6. Ignoring sandbox/approval mode transitions mid-session.
7. Not separating plan/output channels (human-readable vs machine-readable).

## 12) Reference index

### Codex docs + discussions

- https://developers.openai.com/codex/config-advanced
- https://developers.openai.com/codex/guides/agents-md
- https://developers.openai.com/codex/noninteractive
- https://developers.openai.com/codex/mcp
- https://developers.openai.com/codex/multi-agent
- https://github.com/openai/codex/discussions/7296
- https://github.com/openai/codex/discussions/7355
- https://github.com/openai/codex/discussions/5471

### Claude Code docs + chatter

- https://code.claude.com/docs/en/cli-reference.md
- https://code.claude.com/docs/en/settings.md
- https://code.claude.com/docs/en/hooks.md
- https://code.claude.com/docs/en/slash-commands.md
- https://code.claude.com/docs/en/mcp.md
- https://x.com/claude_code/status/1952978675881918594
- https://x.com/claude_code/status/1955210320244326460
- https://x.com/claude_code/status/1955470374579081522

### OpenCode docs + chatter

- https://opencode.ai/docs/permissions/
- https://opencode.ai/docs/commands/
- https://opencode.ai/docs/config/
- https://opencode.ai/docs/cli/
- https://opencode.ai/docs/tui/
- https://x.com/opencode/status/2026553685468135886
- https://x.com/ollama/status/2022018134186791177

### Qwen Code docs + chatter

- https://github.com/QwenLM/qwen-code-docs/blob/main/website/content/en/users/features/approval-mode.md
- https://github.com/QwenLM/qwen-code-docs/blob/main/website/content/en/users/features/checkpointing.md
- https://github.com/QwenLM/qwen-code-docs/blob/main/website/content/en/users/features/commands.md
- https://github.com/QwenLM/qwen-code-docs/blob/main/website/content/en/users/features/headless.md
- https://github.com/QwenLM/qwen-code-docs/blob/main/website/content/en/users/features/mcp.md
- https://github.com/QwenLM/qwen-code-docs/blob/main/website/content/en/users/features/sandbox.md
- https://x.com/Alibaba_Qwen/status/1953835877555151134
- https://x.com/Alibaba_Qwen/status/1959170659583476026

### External REPL / terminal orchestration

- https://github.com/tmux/tmux/blob/master/tmux.1
- https://github.com/tmux/tmux/wiki/Control-Mode
- https://github.com/tmux/tmux/wiki/Clipboard
- https://github.com/tmuxinator/tmuxinator
- https://github.com/zellij-org/zellij-org.github.io/blob/main/docs/src/controlling-zellij-through-cli.md
- https://github.com/zellij-org/zellij-org.github.io/blob/main/content/tutorials/layouts.md
- https://github.com/zellij-org/zellij-org.github.io/blob/main/docs/src/options.md
- https://github.com/watchexec/watchexec
- https://www.entrproject.org/
- https://github.com/direnv/direnv
- https://github.com/junegunn/fzf
- https://github.com/neovim/neovim/blob/master/runtime/doc/terminal.txt
- https://github.com/neovim/neovim/blob/master/runtime/doc/job_control.txt
- https://github.com/microsoft/vscode-docs/blob/main/docs/debugtest/tasks.md

### X thread aggregation used for synthesis

- https://threadreaderapp.com/thread/1916204585787324441.html
