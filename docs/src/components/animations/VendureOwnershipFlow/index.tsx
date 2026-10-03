import React, { useCallback, useEffect, useRef } from "react";
import { animate, stagger } from "animejs";
import { useAnimationSlide } from "@site/src/hooks/useAnimationSlide";
import styles from "./styles.module.css";

function DatabaseIcon() {
    return (
        <svg viewBox="0 0 24 24" aria-hidden="true">
            <ellipse cx="12" cy="5" rx="7" ry="3" />
            <path d="M5 5v10c0 1.7 3.1 3 7 3s7-1.3 7-3V5" />
            <path d="M5 10c0 1.7 3.1 3 7 3s7-1.3 7-3" />
        </svg>
    );
}

function MagicIcon() {
    return (
        <svg viewBox="0 0 24 24" aria-hidden="true">
            <path d="M5 19 17.5 6.5" />
            <path d="m14.5 4 5.5 5.5" />
            <path d="m13.4 10.6-4-4" />
            <path d="M6 4v3" />
            <path d="M4.5 5.5h3" />
            <path d="M19 14v3" />
            <path d="M17.5 15.5h3" />
        </svg>
    );
}

function TagIcon() {
    return (
        <svg viewBox="0 0 24 24" aria-hidden="true">
            <path d="M4 12.4 12.4 4H20v7.6L11.6 20 4 12.4Z" />
            <circle cx="16.8" cy="7.2" r="1.4" />
            <path d="m13.8 14.4 2 2 4-4.2" />
        </svg>
    );
}

export default function VendureOwnershipFlow() {
    const { isPlaying, onComplete } = useAnimationSlide();
    const diagramRef = useRef<HTMLDivElement>(null);
    const outputValueRef = useRef<HTMLSpanElement>(null);
    const timeoutRefs = useRef<ReturnType<typeof setTimeout>[]>([]);

    const clearAllTimeouts = useCallback(() => {
        timeoutRefs.current.forEach(clearTimeout);
        timeoutRefs.current = [];
    }, []);

    const resetAnimation = useCallback(() => {
        clearAllTimeouts();

        const root = diagramRef.current;
        if (!root) return;

        root.querySelectorAll<HTMLElement>("[data-flow-node]").forEach(
            (node) => {
                node.style.opacity = "0";
                node.style.transform = "translateY(18px)";
            },
        );

        root.querySelectorAll<HTMLElement>("[data-engine-node]").forEach(
            (node) => {
                node.style.opacity = "0";
                node.style.transform = "translateY(18px)";
            },
        );

        root.querySelectorAll<HTMLElement>("[data-result-node]").forEach(
            (node) => {
                node.style.opacity = "0";
                node.style.transform = "translateY(18px) scale(0.96)";
            },
        );

        root.querySelectorAll<HTMLElement>("[data-table-line]").forEach(
            (node) => {
                node.style.opacity = "0";
                node.style.transform = "translateY(10px)";
            },
        );

        if (outputValueRef.current) {
            outputValueRef.current.textContent = "800";
        }
    }, [clearAllTimeouts]);

    useEffect(() => {
        if (!isPlaying) {
            resetAnimation();
            return;
        }

        const root = diagramRef.current;
        if (!root) return;

        const addTimeout = (fn: () => void, delay: number) => {
            const id = setTimeout(fn, delay);
            timeoutRefs.current.push(id);
            return id;
        };

        animate(root.querySelector(`.${styles.sourceCard}`), {
            opacity: 1,
            translateY: 0,
            duration: 900,
            ease: "outExpo",
        });

        addTimeout(() => {
            animate(root.querySelectorAll("[data-table-line]"), {
                opacity: 1,
                translateY: 0,
                duration: 760,
                delay: stagger(170),
                ease: "outExpo",
            });
        }, 420);

        addTimeout(() => {
            animate(root.querySelector(`.${styles.engineCard}`), {
                opacity: 1,
                translateY: 0,
                duration: 1050,
                ease: "outExpo",
            });
        }, 1280);

        addTimeout(() => {
            if (outputValueRef.current) {
                outputValueRef.current.textContent = "1200";
            }

            animate(root.querySelector(`.${styles.resultCard}`), {
                opacity: 1,
                translateY: 0,
                scale: [0.96, 1],
                duration: 1100,
                ease: "outExpo",
            });
        }, 2260);

        addTimeout(() => {
            if (onComplete) onComplete();
        }, 3700);

        return () => clearAllTimeouts();
    }, [clearAllTimeouts, isPlaying, onComplete, resetAnimation]);

    return (
        <div
            ref={diagramRef}
            className={styles.figure}
            aria-label="Vendure catalog value, Superposition condition and override, and storefront result"
        >
            <div className={styles.topGrid}>
                <section
                    className={`${styles.card} ${styles.sourceCard}`}
                    data-flow-node
                >
                    <span className={`${styles.eyebrow} ${styles.vendureTone}`}>
                        Vendure (Source of Truth)
                    </span>

                    <div className={styles.cardTitle}>
                        <span
                            className={`${styles.iconBubble} ${styles.databaseBubble}`}
                        >
                            <DatabaseIcon />
                        </span>
                        <strong>Catalog Database</strong>
                    </div>

                    <div className={styles.fieldTable}>
                        <div className={styles.tableHeader} data-table-line>
                            <span>Field</span>
                            <span>Value</span>
                        </div>
                        <div className={styles.tableRow} data-table-line>
                            <span>customFields.bulkPrice</span>
                            <span>800</span>
                        </div>
                    </div>
                </section>

                <section
                    className={`${styles.card} ${styles.engineCard}`}
                    data-engine-node
                >
                    <div className={styles.engineHeading}>
                        <span
                            className={`${styles.iconBubble} ${styles.magicBubble}`}
                        >
                            <MagicIcon />
                        </span>
                        <span
                            className={`${styles.eyebrow} ${styles.engineTone}`}
                        >
                            Superposition
                        </span>
                    </div>

                    <div className={styles.engineRule}>
                        <div className={styles.ruleRow}>
                            <span className={styles.ruleLabel}>Condition</span>
                            <span className={styles.ruleCodeList}>
                                <code>channel = b2b</code>
                                <code>currency = INR</code>
                            </span>
                        </div>

                        <div className={styles.ruleRow}>
                            <span className={styles.ruleLabel}>Override</span>
                            <span className={styles.ruleCodeList}>
                                <code className={styles.overrideCode}>
                                    bulkPrice = <strong>1200</strong>
                                </code>
                            </span>
                        </div>
                    </div>
                </section>
            </div>

            <section
                className={`${styles.resultCard}`}
                data-result-node
                aria-label="Storefront result"
            >
                <span className={`${styles.eyebrow} ${styles.engineTone}`}>
                    Storefront result
                </span>

                <div className={styles.resultBody}>
                    <span className={styles.resultIcon}>
                        <TagIcon />
                    </span>
                    <strong>
                        resolvedBulkPrice ={" "}
                        <span ref={outputValueRef} className={styles.outputValue}>
                            800
                        </span>
                    </strong>
                </div>
            </section>
        </div>
    );
}
