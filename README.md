<p align="center"><code>npm i -g @openai/codex</code><br />or <code>brew install --cask codex</code></p>
<p align="center"><strong>Codex CLI</strong> is a coding agent from OpenAI that runs locally on your computer.
<p align="center">
  <img src="https://github.com/openai/codex/blob/main/.github/codex-cli-splash.png" alt="Codex CLI splash" width="80%" />
</p>
</br>
If you want Codex in your code editor (VS Code, Cursor, Windsurf), <a href="https://developers.openai.com/codex/ide">install in your IDE.</a>
</br>If you want the desktop app experience, run <code>codex app</code> or visit <a href="https://chatgpt.com/codex?app-landing-page=true">the Codex App page</a>.
</br>If you are looking for the <em>cloud-based agent</em> from OpenAI, <strong>Codex Web</strong>, go to <a href="https://chatgpt.com/codex">chatgpt.com/codex</a>.</p>

---

## Quickstart

### Installing and running Codex CLI

Install globally with your preferred package manager:

```shell
# Install using npm
npm install -g @openai/codex
```

```shell
# Install using Homebrew
brew install --cask codex
```

Then simply run `codex` to get started.

<details>
<summary>You can also go to the <a href="https://github.com/openai/codex/releases/latest">latest GitHub Release</a> and download the appropriate binary for your platform.</summary>

Each GitHub Release contains many executables, but in practice, you likely want one of these:

- macOS
  - Apple Silicon/arm64: `codex-aarch64-apple-darwin.tar.gz`
  - x86_64 (older Mac hardware): `codex-x86_64-apple-darwin.tar.gz`
- Linux
  - x86_64: `codex-x86_64-unknown-linux-musl.tar.gz`
  - arm64: `codex-aarch64-unknown-linux-musl.tar.gz`

Each archive contains a single entry with the platform baked into the name (e.g., `codex-x86_64-unknown-linux-musl`), so you likely want to rename it to `codex` after extracting it.

</details>

### Using Codex with your ChatGPT plan

Run `codex` and select **Sign in with ChatGPT**. We recommend signing into your ChatGPT account to use Codex as part of your Plus, Pro, Team, Edu, or Enterprise plan. [Learn more about what's included in your ChatGPT plan](https://help.openai.com/en/articles/11369540-codex-in-chatgpt).

You can also use Codex with an API key, but this requires [additional setup](https://developers.openai.com/codex/auth#sign-in-with-an-api-key).

## Docs

- [**Codex Documentation**](https://developers.openai.com/codex)
- [**Contributing**](./docs/contributing.md)
- [**Installing & building**](./docs/install.md)
- [**Open source fund**](./docs/open-source-fund.md)

## Fork Notes: Native Agent Teams

This fork ([DMontgomery40/codex](https://github.com/DMontgomery40/codex)) adds first-class multi-agent team workflows to the Codex CLI. Upstream `openai/codex` only provides basic subagent primitives (`spawn_agent`, `send_input`, `resume_agent`, `wait`, `close_agent`). This fork builds on those primitives to provide team orchestration, task management, and automatic teammate notifications.

### Features

**Team orchestration tools** -- Spawn, manage, and coordinate multi-agent teams:
- `spawn_team`, `list_teams`, `get_team`, `team_member_status`
- `team_message`, `team_broadcast`
- `wait_team`, `close_team`, `team_cleanup`

**Task management tools** -- File-backed task lists scoped per team:
- `task_create`, `task_list`, `task_get`, `task_update`
- Tasks support status tracking, ownership, dependencies (`blocks`/`blocked_by`), and metadata

**Automatic idle notifications** -- When a teammate finishes a turn, the team lead automatically receives a notification via `inject_user_message_without_turn`. This is centralized in the agent control event flow (not per-tool callbacks), so it fires regardless of how the member was spawned.

**Team state persistence** -- Teams persist to disk under `{codex_home}/teams/` as JSON config files. On restart, teams are restored with their member lists. Task data persists under `{codex_home}/tasks/<team-name>/` with cross-platform file locking.

**TUI keybinding** -- Press `Shift+Down` to open the agent picker (in addition to `/agent` slash command).

### Enabling

Set `multi_agent = true` in your `~/.codex/config.toml` under `[features]`:

```toml
[features]
multi_agent = true
```

### `/team` slash command

```text
/team <create|list|members|send|broadcast|wait|close|cleanup> [json-args]
```

### Python overlay bypass

If you previously used the Python PTY overlay (`~/.codex/agent-teams/`), you can bypass it and use native team tools exclusively:

```shell
export CODEX_NATIVE_TEAMS=1
```

This repository is licensed under the [Apache-2.0 License](LICENSE).
