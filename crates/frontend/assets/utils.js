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

/// Returns a `CompletionItemProvider` https://microsoft.github.io/monaco-editor/docs.html#interfaces/languages.CompletionItemProvider.html
export function newSuggestionsProvider(suggestions) {
    if (typeof suggestions !== "object" || !Array.isArray(suggestions)) {
        throw new Error(`${suggestions} is not an array!`);
    }

    let triggers = [];
    for (const s of suggestions) {
        switch (typeof s) {
            case "string":
                triggers.push('"');
                triggers.push("'");
                break;
            // Not sure if we should provide completions in such cases...
            case "object":
                if (s !== null) {
                    if (Array.isArray(s)) {
                        triggers.push("[");
                    } else {
                        triggers.push("{");
                    }
                } else if (s == null) {
                    triggers.push("n");
                }
                break;
            case "number":
            // While technically not part of JsonSchema,
            // if we can, then why not.
            case "bigint":
                const d = s.toString()[0];
                triggers.push(d)
                break;
            // Screw these.
            case "undefined":
            case "function":
            case "symbol":
            default:
                console.debug(`Un-supported type in suggestions: ${typeof s}, skipping.`);
        }
    }
    // Just in case duplicates lead to some bug.
    triggers = [...new Set(triggers)];
    console.debug(`Trigger characters for suggestions: ${triggers}`);

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
