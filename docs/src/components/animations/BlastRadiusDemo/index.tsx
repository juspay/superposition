import React, { useEffect, useRef, useCallback } from "react";
import { animate } from "animejs";
import { useAnimationSlide } from "@site/src/hooks/useAnimationSlide";
import styles from "./styles.module.css";

export default function BlastRadiusDemo() {
    const { isPlaying, onComplete } = useAnimationSlide();
    const titleRef = useRef<HTMLHeadingElement>(null);
    const subtitleRef = useRef<HTMLParagraphElement>(null);
    const traditionalRef = useRef<HTMLDivElement>(null);
    const superpositionRef = useRef<HTMLDivElement>(null);
    const dividerRef = useRef<HTMLDivElement>(null);
    const explanationRef = useRef<HTMLDivElement>(null);
    const traditionalUsersRef = useRef<HTMLDivElement>(null);
    const superpositionUsersRef = useRef<HTMLDivElement>(null);
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
        if (traditionalRef.current) {
            traditionalRef.current.style.opacity = "0";
            traditionalRef.current.style.transform = "translateX(-50px)";
        }
        if (superpositionRef.current) {
            superpositionRef.current.style.opacity = "0";
            superpositionRef.current.style.transform = "translateX(50px)";
        }
        if (dividerRef.current) {
            dividerRef.current.style.opacity = "0";
        }
        if (explanationRef.current) {
            explanationRef.current.style.opacity = "0";
            explanationRef.current.style.transform = "translateY(20px)";
        }
        if (traditionalUsersRef.current) {
            const users = traditionalUsersRef.current.querySelectorAll(
                `.${styles.userIcon}`,
            );
            users.forEach((user) => user.classList.remove(styles.affected));
        }
        if (superpositionUsersRef.current) {
            const users = superpositionUsersRef.current.querySelectorAll(
                `.${styles.userIcon}`,
            );
            users.forEach((user) => user.classList.remove(styles.safeAffected));
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

        // Traditional panel
        addTimeout(() => {
            animate(traditionalRef.current, {
                opacity: 1,
                translateX: 0,
                duration: 800,
                ease: "outExpo",
            });
        }, 400);

        // Divider
        addTimeout(() => {
            animate(dividerRef.current, {
                opacity: 1,
                duration: 400,
                ease: "outExpo",
            });
        }, 600);

        // Superposition panel
        addTimeout(() => {
            animate(superpositionRef.current, {
                opacity: 1,
                translateX: 0,
                duration: 800,
                ease: "outExpo",
            });
        }, 800);

        // Animate all users turning red in traditional panel
        addTimeout(() => {
            if (traditionalUsersRef.current) {
                const users = traditionalUsersRef.current.querySelectorAll(
                    `.${styles.userIcon}`,
                );
                users.forEach((user, i) => {
                    setTimeout(() => {
                        user.classList.add(styles.affected);
                    }, i * 80);
                });
            }
        }, 1500);

        // Animate just one user in superposition panel
        addTimeout(() => {
            if (superpositionUsersRef.current) {
                const users = superpositionUsersRef.current.querySelectorAll(
                    `.${styles.userIcon}`,
                );
                if (users[0]) {
                    users[0].classList.add(styles.safeAffected);
                }
            }
        }, 2200);

        // Explanation
        addTimeout(() => {
            animate(explanationRef.current, {
                opacity: 1,
                translateY: 0,
                duration: 600,
                ease: "outExpo",
            });
        }, 2800);

        // Complete
        addTimeout(() => {
            if (onComplete) onComplete();
        }, 3600);

        return () => clearAllTimeouts();
    }, [isPlaying, onComplete, clearAllTimeouts, resetAnimation]);

    return (
        <div className={styles.slide}>
            <h2 ref={titleRef} className={styles.title}>
                Problem #3: Blast Radius
            </h2>
            <p ref={subtitleRef} className={styles.subtitle}>
                Config changes hit 100% of users instantly
            </p>

            <div className={styles.blastDemo}>
                <div className={styles.comparison}>
                    <div
                        ref={traditionalRef}
                        className={`${styles.panel} ${styles.traditional}`}
                    >
                        <h3>Traditional Config Systems</h3>
                        <div className={styles.deployVisual}>
                            <div className={styles.deploySwitch}>
                                <span className={styles.switchLabel}>
                                    Deploy
                                </span>
                                <div className={styles.switchToggle}>
                                    <div className={styles.toggleTrack}>
                                        <div className={styles.toggleKnob} />
                                    </div>
                                </div>
                            </div>
                            <div className={styles.impactArrow}>
                                <svg
                                    viewBox="0 0 40 80"
                                    fill="none"
                                    stroke="currentColor"
                                    strokeWidth="2"
                                >
                                    <path d="M20 0v60M20 60l-10-10M20 60l10-10" />
                                </svg>
                            </div>
                            <div
                                ref={traditionalUsersRef}
                                className={styles.userGrid}
                            >
                                {[...Array(9)].map((_, i) => (
                                    <span key={i} className={styles.userIcon}>
                                        👤
                                    </span>
                                ))}
                            </div>
                            <div
                                className={`${styles.impactLabel} ${styles.danger}`}
                            >
                                100% impacted instantly
                            </div>
                        </div>
                    </div>

                    <div ref={dividerRef} className={styles.divider}>
                        <span>vs</span>
                    </div>

                    <div
                        ref={superpositionRef}
                        className={`${styles.panel} ${styles.superposition}`}
                    >
                        <h3>With Superposition</h3>
                        <div className={styles.deployVisual}>
                            <div className={styles.gradualRollout}>
                                <div className={styles.rolloutBar}>
                                    <div
                                        className={styles.rolloutFill}
                                        style={{ width: "10%" }}
                                    />
                                </div>
                                <span className={styles.rolloutPercent}>
                                    10%
                                </span>
                            </div>
                            <div
                                className={`${styles.impactArrow} ${styles.safe}`}
                            >
                                <svg
                                    viewBox="0 0 40 80"
                                    fill="none"
                                    stroke="currentColor"
                                    strokeWidth="2"
                                >
                                    <path d="M20 0v60M20 60l-10-10M20 60l10-10" />
                                </svg>
                            </div>
                            <div
                                ref={superpositionUsersRef}
                                className={styles.userGrid}
                            >
                                {[...Array(9)].map((_, i) => (
                                    <span key={i} className={styles.userIcon}>
                                        👤
                                    </span>
                                ))}
                            </div>
                            <div
                                className={`${styles.impactLabel} ${styles.success}`}
                            >
                                Gradual, controlled rollout
                            </div>
                        </div>
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
                        <path d="M13 10V3L4 14h7v7l9-11h-7z" />
                    </svg>
                </div>
                <p>
                    Database-style "flip the switch" ={" "}
                    <strong>entire user base at risk</strong>
                </p>
            </div>
        </div>
    );
}
