import React, { useEffect, useRef, useCallback } from "react";
import { animate } from "animejs";
import { useAnimationSlide } from "@site/src/hooks/useAnimationSlide";
import styles from "./styles.module.css";

export default function InvalidValuesDemo() {
    const { isPlaying, onComplete } = useAnimationSlide();
    const titleRef = useRef<HTMLHeadingElement>(null);
    const subtitleRef = useRef<HTMLParagraphElement>(null);
    const validatorRef = useRef<HTMLDivElement>(null);
    const checkItemsRef = useRef<HTMLDivElement>(null);
    const consequenceRef = useRef<HTMLDivElement>(null);
    const explanationRef = useRef<HTMLDivElement>(null);
    const urlTextRef = useRef<HTMLSpanElement>(null);
    const urlInputRef = useRef<HTMLDivElement>(null);
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
        if (validatorRef.current) {
            validatorRef.current.style.opacity = "0";
            validatorRef.current.style.transform = "translateY(50px)";
        }
        if (checkItemsRef.current) {
            const items = checkItemsRef.current.querySelectorAll(`.${styles.checkItem}`);
            items.forEach((item) => {
                item.classList.remove(styles.visible);
            });
        }
        if (consequenceRef.current) {
            consequenceRef.current.classList.remove(styles.visible);
        }
        if (explanationRef.current) {
            explanationRef.current.style.opacity = "0";
            explanationRef.current.style.transform = "translateY(20px)";
        }
        if (urlTextRef.current) {
            urlTextRef.current.classList.remove(styles.changed);
        }
        if (urlInputRef.current) {
            urlInputRef.current.classList.remove(styles.invalid);
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

        // Validator
        addTimeout(() => {
            animate(validatorRef.current, {
                opacity: 1,
                translateY: 0,
                duration: 800,
                ease: "outExpo",
            });
        }, 400);

        // First check item (type: string ✓)
        addTimeout(() => {
            if (checkItemsRef.current) {
                const firstCheck = checkItemsRef.current.querySelector(`[data-check="0"]`);
                if (firstCheck) firstCheck.classList.add(styles.visible);
            }
        }, 1200);

        // Change URL to invalid
        addTimeout(() => {
            if (urlTextRef.current) urlTextRef.current.classList.add(styles.changed);
            if (urlInputRef.current) urlInputRef.current.classList.add(styles.invalid);
        }, 1800);

        // Show validation failure checks
        addTimeout(() => {
            if (checkItemsRef.current) {
                const check = checkItemsRef.current.querySelector(`[data-check="1"]`);
                if (check) check.classList.add(styles.visible);
            }
        }, 2200);

        addTimeout(() => {
            if (checkItemsRef.current) {
                const check = checkItemsRef.current.querySelector(`[data-check="2"]`);
                if (check) check.classList.add(styles.visible);
            }
        }, 2500);

        addTimeout(() => {
            if (checkItemsRef.current) {
                const check = checkItemsRef.current.querySelector(`[data-check="3"]`);
                if (check) check.classList.add(styles.visible);
            }
        }, 2800);

        // Show consequence
        addTimeout(() => {
            if (consequenceRef.current) {
                consequenceRef.current.classList.add(styles.visible);
            }
        }, 3200);

        // Explanation
        addTimeout(() => {
            animate(explanationRef.current, {
                opacity: 1,
                translateY: 0,
                duration: 600,
                ease: "outExpo",
            });
        }, 3600);

        // Complete
        addTimeout(() => {
            if (onComplete) onComplete();
        }, 4400);

        return () => clearAllTimeouts();
    }, [isPlaying, onComplete, clearAllTimeouts, resetAnimation]);

    const checks = [
        { icon: "✓", text: "Type: String", pass: true },
        { icon: "✗", text: "Format: Invalid URL", pass: false },
        { icon: "✗", text: 'Protocol: "htps" not recognized', pass: false },
        { icon: "✗", text: "Contains illegal space character", pass: false },
    ];

    return (
        <div className={styles.slide}>
            <h2 ref={titleRef} className={styles.title}>
                Problem #2: Invalid Values
            </h2>
            <p ref={subtitleRef} className={styles.subtitle}>
                Correct type ≠ Correct value
            </p>

            <div className={styles.validationDemo}>
                <div ref={validatorRef} className={styles.validator}>
                    <div className={styles.validatorHeader}>
                        <span className={styles.validatorLabel}>API Endpoint Config</span>
                        <span className={styles.typeBadge}>type: string ✓</span>
                    </div>

                    <div className={styles.inputContainer}>
                        <span className={styles.inputLabel}>webhook_url</span>
                        <div ref={urlInputRef} className={styles.urlInput}>
                            <span ref={urlTextRef} className={styles.urlText}>
                                <span className={styles.urlValid}>https://api.payment.com/webhook</span>
                                <span className={styles.urlInvalid}>htps://api.payment .com/webhook</span>
                            </span>
                            <span className={styles.cursorBlink}></span>
                        </div>
                    </div>

                    <div ref={checkItemsRef} className={styles.validationChecks}>
                        {checks.map((check, index) => (
                            <div
                                key={index}
                                data-check={index}
                                className={`${styles.checkItem} ${check.pass ? styles.pass : styles.fail}`}
                            >
                                <span className={styles.checkIcon}>{check.icon}</span>
                                <span>{check.text}</span>
                            </div>
                        ))}
                    </div>
                </div>

                <div ref={consequenceRef} className={styles.consequence}>
                    <div className={styles.consequenceIcon}>
                        <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                            <path d="M18.364 18.364A9 9 0 005.636 5.636m12.728 12.728A9 9 0 015.636 5.636m12.728 12.728L5.636 5.636" />
                        </svg>
                    </div>
                    <div className={styles.consequenceText}>
                        <span className={styles.consequenceTitle}>Payment webhooks silently failing</span>
                        <span className={styles.consequenceDetail}>Customers charged but orders not confirmed</span>
                    </div>
                </div>
            </div>

            <div ref={explanationRef} className={styles.explanation}>
                <div className={styles.explanationIcon}>
                    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                        <path d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
                    </svg>
                </div>
                <p>
                    Type checks alone are <strong>not enough</strong> — values need semantic validation
                </p>
            </div>
        </div>
    );
}
