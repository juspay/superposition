import React, { useEffect, useRef, useCallback } from "react";
import { animate } from "animejs";
import { useAnimationSlide } from "@site/src/hooks/useAnimationSlide";
import styles from "./styles.module.css";

export default function TypeSafetyDemo() {
    const { isPlaying, onComplete } = useAnimationSlide();
    const titleRef = useRef<HTMLHeadingElement>(null);
    const subtitleRef = useRef<HTMLParagraphElement>(null);
    const editorRef = useRef<HTMLDivElement>(null);
    const arrowRef = useRef<HTMLDivElement>(null);
    const cardsRef = useRef<HTMLDivElement>(null);
    const explanationRef = useRef<HTMLDivElement>(null);
    const typeChangeRef = useRef<HTMLSpanElement>(null);
    const highlightLineRef = useRef<HTMLDivElement>(null);
    const timeoutRefs = useRef<NodeJS.Timeout[]>([]);

    const clearAllTimeouts = useCallback(() => {
        timeoutRefs.current.forEach(clearTimeout);
        timeoutRefs.current = [];
    }, []);

    const resetAnimation = useCallback(() => {
        clearAllTimeouts();

        // Reset all refs to initial hidden state
        [titleRef, subtitleRef, editorRef, explanationRef].forEach((ref) => {
            if (ref.current) {
                ref.current.style.opacity = "0";
                ref.current.style.transform = "translateY(30px)";
            }
        });
        if (arrowRef.current) arrowRef.current.style.opacity = "0";
        if (cardsRef.current) {
            cardsRef.current.style.opacity = "0";
            cardsRef.current.style.transform = "translateX(30px)";
        }
        if (typeChangeRef.current) {
            typeChangeRef.current.classList.remove(styles.changed);
        }
        if (highlightLineRef.current) {
            highlightLineRef.current.classList.remove(styles.highlightLine);
        }
    }, [clearAllTimeouts]);

    useEffect(() => {
        if (!isPlaying) {
            resetAnimation();
            return;
        }

        // Set initial states
        [titleRef, subtitleRef, editorRef, explanationRef].forEach((ref) => {
            if (ref.current) {
                ref.current.style.opacity = "0";
                ref.current.style.transform = "translateY(30px)";
            }
        });
        if (arrowRef.current) arrowRef.current.style.opacity = "0";
        if (cardsRef.current) {
            cardsRef.current.style.opacity = "0";
            cardsRef.current.style.transform = "translateX(30px)";
        }

        // Helper to track timeouts
        const addTimeout = (fn: () => void, delay: number) => {
            const id = setTimeout(fn, delay);
            timeoutRefs.current.push(id);
            return id;
        };

        // Animation timeline
        animate(titleRef.current, {
            opacity: 1,
            translateY: 0,
            duration: 800,
            ease: "outExpo",
        });

        addTimeout(() => {
            animate(subtitleRef.current, {
                opacity: 1,
                translateY: 0,
                duration: 600,
                ease: "outExpo",
            });
        }, 200);

        addTimeout(() => {
            animate(editorRef.current, {
                opacity: 1,
                translateY: 0,
                duration: 800,
                ease: "outExpo",
            });
        }, 400);

        addTimeout(() => {
            animate(arrowRef.current, {
                opacity: 1,
                duration: 500,
                ease: "outExpo",
            });
        }, 800);

        addTimeout(() => {
            animate(cardsRef.current, {
                opacity: 1,
                translateX: 0,
                duration: 600,
                ease: "outExpo",
            });
        }, 1000);

        addTimeout(() => {
            if (typeChangeRef.current)
                typeChangeRef.current.classList.add(styles.changed);
            if (highlightLineRef.current)
                highlightLineRef.current.classList.add(styles.highlightLine);
        }, 2000);

        addTimeout(() => {
            animate(explanationRef.current, {
                opacity: 1,
                translateY: 0,
                duration: 600,
                ease: "outExpo",
            });
        }, 2500);

        // Signal completion after animation finishes (2500 + 600 + buffer)
        addTimeout(() => {
            if (onComplete) onComplete();
        }, 3200);

        return () => clearAllTimeouts();
    }, [isPlaying, onComplete, clearAllTimeouts, resetAnimation]);

    return (
        <div className={styles.slide}>
            <h2 ref={titleRef} className={styles.title}>
                Problem #1: Type Mismatch
            </h2>
            <p ref={subtitleRef} className={styles.subtitle}>
                Wrong types silently break production
            </p>

            <div className={styles.demo}>
                <div ref={editorRef} className={styles.editor}>
                    <div className={styles.editorHeader}>
                        <span className={`${styles.dot} ${styles.red}`} />
                        <span className={`${styles.dot} ${styles.yellow}`} />
                        <span className={`${styles.dot} ${styles.green}`} />
                        <span className={styles.filename}>config.json</span>
                    </div>
                    <div className={styles.editorBody}>
                        <div className={styles.codeLine}>
                            <span className={styles.lineNum}>1</span>
                            {"{"}
                        </div>
                        <div ref={highlightLineRef} className={styles.codeLine}>
                            <span className={styles.lineNum}>2</span>
                            <span className={styles.jsonKey}>
                                "surge_factor"
                            </span>
                            :{" "}
                            <span
                                ref={typeChangeRef}
                                className={styles.typeChange}
                            >
                                <span className={styles.valueBefore}>1.5</span>
                                <span className={styles.valueAfter}>
                                    "high"
                                </span>
                            </span>
                        </div>
                        <div className={styles.codeLine}>
                            <span className={styles.lineNum}>3</span>
                            {"}"}
                        </div>
                    </div>
                </div>

                <div ref={arrowRef} className={styles.arrow}>
                    <svg
                        viewBox="0 0 60 24"
                        fill="none"
                        stroke="currentColor"
                        strokeWidth="2"
                    >
                        <path d="M0 12h50M50 12l-8-8M50 12l-8 8" />
                    </svg>
                </div>

                <div ref={cardsRef} className={styles.results}>
                    <div className={`${styles.card} ${styles.before}`}>
                        <span className={styles.label}>Expected</span>
                        <code>
                            price = base * <strong>1.5</strong>
                        </code>
                        <span className={styles.output}>₹150</span>
                    </div>
                    <div className={`${styles.card} ${styles.after}`}>
                        <span className={styles.label}>Actual</span>
                        <code>
                            price = base * <strong>"high"</strong>
                        </code>
                        <span className={`${styles.output} ${styles.error}`}>
                            NaN
                        </span>
                        <span className={styles.badge}>CRASH</span>
                    </div>
                </div>
            </div>

            <div ref={explanationRef} className={styles.explanation}>
                <div className={styles.explanationIcon}>
                    <svg
                        viewBox="0 0 24 24"
                        fill="none"
                        stroke="currentColor"
                        strokeWidth="2"
                    >
                        <circle cx="12" cy="12" r="10" />
                        <path d="M12 8v4M12 16h.01" />
                    </svg>
                </div>
                <p>
                    No schema validation ={" "}
                    <strong>string passed where number expected</strong>
                </p>
            </div>
        </div>
    );
}
