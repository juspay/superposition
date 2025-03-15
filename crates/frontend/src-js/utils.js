/**
 * Sensible conversion of javascript value to string.
 * @param {*} v
 * @returns {string}
 */
function toStr(v) {
    switch (typeof v) {
        case "string":
            return v;
        case "object":
            return JSON.stringify(v);
        default:
            return v.toString();
    }
}

/**
 * Constructor
 * @param {Array} triggers
 * @param {Array} suggestions
 * @returns {CompletionItemProvider} https://microsoft.github.io/monaco-editor/docs.html#interfaces/languages.CompletionItemProvider.html
 */
export function newSuggestionsProvider(triggers, suggestions) {
    if (typeof suggestions !== "object" || !Array.isArray(suggestions)) {
        throw new Error(`suggestions: ${suggestions} is not an array!`);
    }
    if (typeof triggers !== "object" || !Array.isArray(triggers)) {
        throw new Error(`triggers: ${triggers} is not an array!`);
    }
    return {
        provideCompletionItems: function (model, position) {
            return {
                suggestions: suggestions.map((s) => ({
                    // This is the text that shows up in the completion hover.
                    label: toStr(s),
                    // FIXME Coupling w/ private Enum, this can break w/ a newer version of
                    // monaco.
                    kind: 15, // Maps to the `Enum` varaint of `Kind`.
                    insertText: toStr(s),
                    detail: "Enum variant",
                    documentation: "Json Enum",
                })),
            };
        },
        triggerCharacters: triggers,
    };
}

/**
 * @param {string} code - code that replaces everything in the editor
 * @returns {Nothing}
 */
export function setEditorCode(code) {
    const editor = monaco.editor.getEditors()[0];
    editor.setValue(code);
}
