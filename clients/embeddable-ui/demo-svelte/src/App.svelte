<script>
  const navigation = [
    { id: "config", label: "Default Config" },
    { id: "dimensions", label: "Dimensions" },
  ];

  const config = JSON.stringify({
    apiBaseUrl: "/api",
    orgId: "localorg",
    workspace: "dev",
    theme: {
      mode: "light",
      colors: {
        primary: "oklch(0.52 0.18 270)",
        bg: "oklch(0.98 0.006 270)",
        panel: "oklch(0.995 0.003 270)",
        text: "oklch(0.25 0.025 270)",
        muted: "oklch(0.52 0.025 270)",
        border: "oklch(0.9 0.012 270)",
      },
    },
  });

  let active = "config";
</script>

<svelte:head>
  <meta name="theme-color" content="oklch(0.23 0.04 270)" />
</svelte:head>

<div class="app-shell">
  <aside class="sidebar" aria-label="Superposition demo navigation">
    <div class="brand">
      <span class="brand-mark" aria-hidden="true">S</span>
      <span>
        <strong>Superposition</strong>
        <small>Svelte host</small>
      </span>
    </div>

    <nav>
      <p>Manage</p>
      {#each navigation as item}
        <button
          type="button"
          class:active={active === item.id}
          aria-current={active === item.id ? "page" : undefined}
          onclick={() => (active = item.id)}
        >
          {#if item.id === "config"}
            <svg viewBox="0 0 24 24" aria-hidden="true">
              <path d="M4 6.5h16M4 12h16M4 17.5h10" />
            </svg>
          {:else}
            <svg viewBox="0 0 24 24" aria-hidden="true">
              <circle cx="7" cy="7" r="2.5" />
              <circle cx="17" cy="7" r="2.5" />
              <circle cx="12" cy="17" r="2.5" />
              <path d="m9 8.5 2 6m4-6-2 6" />
            </svg>
          {/if}
          <span>{item.label}</span>
        </button>
      {/each}
    </nav>

    <div class="sidebar-footer">
      <span class="status-dot" aria-hidden="true"></span>
      <span><strong>Local API</strong><small>localhost:8081</small></span>
    </div>
  </aside>

  <main>
    <header>
      <div>
        <p class="eyebrow">Workspace</p>
        <h1>{active === "config" ? "Default Config" : "Dimensions"}</h1>
      </div>
      <div class="context" aria-label="Current Superposition context">
        <span><small>Organization</small>localorg</span>
        <span><small>Workspace</small>test</span>
      </div>
    </header>

    <section class="content" aria-label={`${active} manager`}>
      {#if active === "config"}
        <superposition-config-manager {config}></superposition-config-manager>
      {:else}
        <superposition-dimension-manager {config}></superposition-dimension-manager>
      {/if}
    </section>
  </main>
</div>

<style>
  :global(*) {
    box-sizing: border-box;
  }

  :global(html) {
    background: oklch(0.97 0.008 270);
  }

  :global(body) {
    margin: 0;
    min-width: 320px;
    min-height: 100vh;
    color: oklch(0.25 0.025 270);
    background: oklch(0.97 0.008 270);
    font-family:
      Inter, ui-sans-serif, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  }

  :global(button) {
    font: inherit;
  }

  .app-shell {
    display: grid;
    grid-template-columns: 248px minmax(0, 1fr);
    min-height: 100vh;
  }

  .sidebar {
    position: sticky;
    top: 0;
    display: flex;
    flex-direction: column;
    height: 100vh;
    padding: 24px 16px 18px;
    color: oklch(0.93 0.012 270);
    background: oklch(0.23 0.04 270);
  }

  .brand {
    display: flex;
    align-items: center;
    gap: 12px;
    padding: 0 8px 30px;
  }

  .brand-mark {
    display: grid;
    place-items: center;
    width: 34px;
    height: 34px;
    flex: 0 0 auto;
    border-radius: 9px;
    color: oklch(0.99 0.004 270);
    background: oklch(0.62 0.19 270);
    font-weight: 750;
  }

  .brand strong,
  .brand small,
  .sidebar-footer strong,
  .sidebar-footer small {
    display: block;
  }

  .brand strong {
    color: oklch(0.98 0.006 270);
    font-size: 14px;
    letter-spacing: -0.01em;
  }

  .brand small,
  .sidebar-footer small {
    margin-top: 2px;
    color: oklch(0.7 0.025 270);
    font-size: 11px;
  }

  nav p {
    margin: 0 10px 8px;
    color: oklch(0.66 0.035 270);
    font-size: 10px;
    font-weight: 700;
    letter-spacing: 0.11em;
    text-transform: uppercase;
  }

  nav button {
    display: flex;
    align-items: center;
    gap: 11px;
    width: 100%;
    min-height: 42px;
    margin: 2px 0;
    padding: 0 11px;
    border: 0;
    border-radius: 8px;
    color: oklch(0.75 0.025 270);
    background: transparent;
    cursor: pointer;
    font-size: 13px;
    font-weight: 550;
    text-align: left;
    transition:
      color 160ms cubic-bezier(0.22, 1, 0.36, 1),
      background-color 160ms cubic-bezier(0.22, 1, 0.36, 1);
  }

  nav button:hover {
    color: oklch(0.96 0.01 270);
    background: oklch(0.3 0.04 270);
  }

  nav button:focus-visible {
    outline: 2px solid oklch(0.76 0.13 270);
    outline-offset: 2px;
  }

  nav button.active {
    color: oklch(0.97 0.014 270);
    background: oklch(0.36 0.08 270);
  }

  nav svg {
    width: 18px;
    height: 18px;
    fill: none;
    stroke: currentColor;
    stroke-linecap: round;
    stroke-linejoin: round;
    stroke-width: 1.8;
  }

  .sidebar-footer {
    display: flex;
    align-items: center;
    gap: 10px;
    margin-top: auto;
    padding: 14px 10px 0;
    border-top: 1px solid oklch(0.36 0.035 270);
    font-size: 12px;
  }

  .status-dot {
    width: 8px;
    height: 8px;
    border-radius: 50%;
    background: oklch(0.72 0.16 150);
    box-shadow: 0 0 0 3px oklch(0.72 0.16 150 / 0.13);
  }

  main {
    min-width: 0;
  }

  header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    min-height: 88px;
    padding: 18px clamp(22px, 4vw, 52px);
    border-bottom: 1px solid oklch(0.89 0.012 270);
    background: oklch(0.99 0.004 270);
  }

  .eyebrow {
    margin: 0 0 4px;
    color: oklch(0.56 0.025 270);
    font-size: 11px;
    font-weight: 650;
    letter-spacing: 0.08em;
    text-transform: uppercase;
  }

  h1 {
    margin: 0;
    color: oklch(0.23 0.035 270);
    font-size: 22px;
    font-weight: 700;
    letter-spacing: -0.025em;
  }

  .context {
    display: flex;
    gap: 24px;
    color: oklch(0.34 0.025 270);
    font-size: 12px;
    font-weight: 600;
  }

  .context span {
    min-width: 74px;
  }

  .context small {
    display: block;
    margin-bottom: 3px;
    color: oklch(0.58 0.022 270);
    font-size: 10px;
    font-weight: 550;
  }

  .content {
    min-height: calc(100vh - 88px);
    padding: 28px clamp(22px, 4vw, 52px) 48px;
    background: oklch(0.97 0.008 270);
  }

  .content :global(superposition-config-manager),
  .content :global(superposition-dimension-manager) {
    display: block;
    min-height: 520px;
    padding: 24px;
    overflow: hidden;
    border: 1px solid oklch(0.89 0.012 270);
    border-radius: 12px;
    background: oklch(0.99 0.004 270);
    box-shadow: 0 10px 28px oklch(0.28 0.03 270 / 0.055);
  }

  @media (max-width: 760px) {
    .app-shell {
      grid-template-columns: 1fr;
    }

    .sidebar {
      position: static;
      height: auto;
      padding: 16px;
    }

    .brand {
      padding: 0 2px 14px;
    }

    nav {
      display: flex;
      gap: 6px;
    }

    nav p,
    .sidebar-footer {
      display: none;
    }

    nav button {
      width: auto;
      min-height: 38px;
    }

    header {
      align-items: flex-start;
      min-height: 100px;
      padding: 16px 20px;
    }

    .context {
      gap: 14px;
    }

    .content {
      min-height: 0;
      padding: 18px 12px 28px;
    }

    .content :global(superposition-config-manager),
    .content :global(superposition-dimension-manager) {
      padding: 16px;
    }
  }

  @media (prefers-reduced-motion: reduce) {
    nav button {
      transition: none;
    }
  }
</style>
