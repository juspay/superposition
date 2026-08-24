import { mount } from "svelte";
import { defineCustomElements } from "superposition-embeddable-ui/browser";
import App from "./App.svelte";

defineCustomElements();

mount(App, {
  target: document.getElementById("app"),
});
