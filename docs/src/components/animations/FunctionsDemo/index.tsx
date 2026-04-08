import React, { useEffect, useRef, useCallback } from "react";
import { animate, stagger } from "animejs";
import { useAnimationSlide } from "@site/src/hooks/useAnimationSlide";
import styles from "./styles.module.css";

export default function FunctionsDemo() {
    const { isPlaying, onComplete } = useAnimationSlide();
    const titleRef = useRef<HTMLHeadingElement>(null);
    const subtitleRef = useRef<HTMLParagraphElement>(null);
    const rowsRef = useRef<HTMLDivElement>(null);
    const timeoutRefs = useRef<ReturnType<typeof setTimeout>[]>([]);

    const clearAllTimeouts = useCallback(() => {
        timeoutRefs.current.forEach(clearTimeout);
        timeoutRefs.current = [];
    }, []);

    const resetAnimation = useCallback(() => {
        clearAllTimeouts();

        if (titleRef.current) {
            titleRef.current.style.opacity = "0";
            titleRef.current.style.transform = "translateY(40px)";
        }
        if (subtitleRef.current) {
            subtitleRef.current.style.opacity = "0";
            subtitleRef.current.style.transform = "translateY(30px)";
        }
        if (rowsRef.current) {
            const targets = rowsRef.current.querySelectorAll(
                `.${styles.target}`,
            );
            targets.forEach((target) => {
                (target as HTMLElement).style.opacity = "0";
                (target as HTMLElement).style.transform = "translateX(-30px)";
                target.classList.remove(styles.active);
            });
            const fnTypes = rowsRef.current.querySelectorAll(
                `.${styles.fnType}`,
            );
            fnTypes.forEach((fn) => {
                (fn as HTMLElement).style.opacity = "0.4";
                (fn as HTMLElement).style.transform = "translateX(20px)";
                fn.classList.remove(styles.active);
            });
            const lines = rowsRef.current.querySelectorAll(`.${styles.fnLine}`);
            lines.forEach((line) => {
                line.classList.remove(styles.active);
                (line as SVGPathElement).style.strokeDashoffset = "100";
            });
        }
    }, [clearAllTimeouts]);

    useEffect(() => {
        if (!isPlaying) {
            resetAnimation();
            return;
        }

        const addTimeout = (fn: () => void, delay: number) => {
            const id = setTimeout(fn, delay);
            timeoutRefs.current.push(id);
            return id;
        };

        // Title
        animate(titleRef.current, {
            opacity: 1,
            translateY: 0,
            duration: 800,
            ease: "outExpo",
        });

        // Subtitle
        addTimeout(() => {
            animate(subtitleRef.current, {
                opacity: 1,
                translateY: 0,
                duration: 600,
                ease: "outExpo",
            });
        }, 200);

        // Animation sequence - row by row
        const rowSequence = [
            { row: "dimensions", functions: ["validate", "compute"] },
            { row: "contexts", functions: ["ctx-validate"] },
            { row: "changes", functions: ["change-validate"] },
        ];

        let baseDelay = 600;

        rowSequence.forEach((seq, seqIndex) => {
            const delay = baseDelay + seqIndex * 1000;

            addTimeout(() => {
                if (!rowsRef.current) return;

                const row = rowsRef.current.querySelector(
                    `[data-row="${seq.row}"]`,
                );
                if (!row) return;

                const targetEl = row.querySelector(`.${styles.target}`);
                const rowLines = row.querySelectorAll(`.${styles.fnLine}`);

                // Animate target appearing
                if (targetEl) {
                    targetEl.classList.add(styles.active);
                    animate(targetEl, {
                        opacity: 1,
                        translateX: 0,
                        duration: 500,
                        ease: "outExpo",
                    });
                }

                // Animate lines after target
                addTimeout(() => {
                    rowLines.forEach((line, lineIndex) => {
                        addTimeout(() => {
                            line.classList.add(styles.active);
                            animate(line, {
                                strokeDashoffset: 0,
                                duration: 400,
                                ease: "outQuart",
                            });
                        }, lineIndex * 150);
                    });
                }, 200);

                // Activate function types after lines
                seq.functions.forEach((fnType, fnIndex) => {
                    addTimeout(
                        () => {
                            const fnEl = rowsRef.current?.querySelector(
                                `[data-fn="${fnType}"]`,
                            );
                            if (fnEl) {
                                fnEl.classList.add(styles.active);
                                animate(fnEl, {
                                    opacity: 1,
                                    translateX: 0,
                                    duration: 500,
                                    ease: "outExpo",
                                });
                            }
                        },
                        400 + fnIndex * 200,
                    );
                });
            }, delay);
        });

        // Complete
        addTimeout(() => {
            if (onComplete) onComplete();
        }, 4000);

        return () => clearAllTimeouts();
    }, [isPlaying, onComplete, clearAllTimeouts, resetAnimation]);

    return (
        <div className={styles.slide}>
            <h2 ref={titleRef} className={styles.title}>
                Functions
            </h2>
            <p ref={subtitleRef} className={styles.subtitle}>
                Custom logic for validation and computation
            </p>

            <div ref={rowsRef} className={styles.fnRows}>
                {/* Row 1: Dimensions → Value Validate & Value Compute */}
                <div className={styles.fnRow} data-row="dimensions">
                    <div className={styles.target} data-target="dimensions">
                        <div className={styles.targetIcon}>
                            <svg
                                viewBox="0 0 24 24"
                                fill="none"
                                stroke="currentColor"
                                strokeWidth="2"
                            >
                                <rect x="3" y="3" width="7" height="7" rx="1" />
                                <rect
                                    x="14"
                                    y="3"
                                    width="7"
                                    height="7"
                                    rx="1"
                                />
                                <rect
                                    x="3"
                                    y="14"
                                    width="7"
                                    height="7"
                                    rx="1"
                                />
                                <rect
                                    x="14"
                                    y="14"
                                    width="7"
                                    height="7"
                                    rx="1"
                                />
                            </svg>
                        </div>
                        <span className={styles.targetLabel}>
                            Dimensions &<br />
                            Default Configs
                        </span>
                    </div>

                    <div className={styles.connector}>
                        <svg viewBox="0 0 80 100" preserveAspectRatio="none">
                            <path
                                className={styles.fnLine}
                                data-to="validate"
                                d="M0,25 C30,25 50,25 80,25"
                            />
                            <path
                                className={styles.fnLine}
                                data-to="compute"
                                d="M0,25 C30,25 50,75 80,75"
                            />
                        </svg>
                    </div>

                    <div className={styles.fnTypeGroup}>
                        <div className={styles.fnType} data-fn="validate">
                            <span
                                className={`${styles.fnTypeBadge} ${styles.validate}`}
                            >
                                Validate
                            </span>
                            <h4 className={styles.fnTypeName}>
                                Value Validate
                            </h4>
                            <p className={styles.fnTypeDesc}>
                                Ensure values meet custom criteria
                            </p>
                            <code className={styles.fnTypeExample}>
                                fn(value) → bool
                            </code>
                        </div>

                        <div className={styles.fnType} data-fn="compute">
                            <span
                                className={`${styles.fnTypeBadge} ${styles.compute}`}
                            >
                                Compute
                            </span>
                            <h4 className={styles.fnTypeName}>Value Compute</h4>
                            <p className={styles.fnTypeDesc}>
                                Transform or derive values dynamically
                            </p>
                            <code className={styles.fnTypeExample}>
                                fn(value) → value
                            </code>
                        </div>
                    </div>
                </div>

                {/* Row 2: Contexts → Context Validate */}
                <div className={styles.fnRow} data-row="contexts">
                    <div className={styles.target} data-target="contexts">
                        <div className={styles.targetIcon}>
                            <svg
                                viewBox="0 0 24 24"
                                fill="none"
                                stroke="currentColor"
                                strokeWidth="2"
                            >
                                <path d="M12 2L2 7l10 5 10-5-10-5z" />
                                <path d="M2 17l10 5 10-5" />
                                <path d="M2 12l10 5 10-5" />
                            </svg>
                        </div>
                        <span className={styles.targetLabel}>Contexts</span>
                    </div>

                    <div className={`${styles.connector} ${styles.single}`}>
                        <svg viewBox="0 0 80 50" preserveAspectRatio="none">
                            <path
                                className={styles.fnLine}
                                data-to="ctx-validate"
                                d="M0,25 C30,25 50,25 80,25"
                            />
                        </svg>
                    </div>

                    <div className={`${styles.fnTypeGroup} ${styles.single}`}>
                        <div className={styles.fnType} data-fn="ctx-validate">
                            <span
                                className={`${styles.fnTypeBadge} ${styles.ctxValidate}`}
                            >
                                Validate
                            </span>
                            <h4 className={styles.fnTypeName}>
                                Context Validate
                            </h4>
                            <p className={styles.fnTypeDesc}>
                                Validate context combinations
                            </p>
                            <code className={styles.fnTypeExample}>
                                fn(context) → bool
                            </code>
                        </div>
                    </div>
                </div>

                {/* Row 3: Change Reason → Change Reason Validate */}
                <div className={styles.fnRow} data-row="changes">
                    <div className={styles.target} data-target="changes">
                        <div className={styles.targetIcon}>
                            <svg
                                viewBox="0 0 24 24"
                                fill="none"
                                stroke="currentColor"
                                strokeWidth="2"
                            >
                                <path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z" />
                                <path d="M14 2v6h6" />
                                <path d="M9 15h6" />
                                <path d="M12 12v6" />
                            </svg>
                        </div>
                        <span className={styles.targetLabel}>
                            Change Reason
                        </span>
                    </div>

                    <div className={`${styles.connector} ${styles.single}`}>
                        <svg viewBox="0 0 80 50" preserveAspectRatio="none">
                            <path
                                className={styles.fnLine}
                                data-to="change-validate"
                                d="M0,25 C30,25 50,25 80,25"
                            />
                        </svg>
                    </div>

                    <div className={`${styles.fnTypeGroup} ${styles.single}`}>
                        <div
                            className={styles.fnType}
                            data-fn="change-validate"
                        >
                            <span
                                className={`${styles.fnTypeBadge} ${styles.changeValidate}`}
                            >
                                Validate
                            </span>
                            <h4 className={styles.fnTypeName}>
                                Change Reason Validate
                            </h4>
                            <p className={styles.fnTypeDesc}>
                                Enforce change documentation rules
                            </p>
                            <code className={styles.fnTypeExample}>
                                fn(reason) → bool
                            </code>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
}
