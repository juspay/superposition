import React, { useEffect, useRef, useCallback } from "react";
import { animate, stagger } from "animejs";
import { useAnimationSlide } from "@site/src/hooks/useAnimationSlide";
import styles from "./styles.module.css";

export default function DefaultConfigs() {
    const { isPlaying, onComplete } = useAnimationSlide();
    const titleRef = useRef<HTMLHeadingElement>(null);
    const subtitleRef = useRef<HTMLParagraphElement>(null);
    const cardsRef = useRef<HTMLDivElement>(null);
    const explanationRef = useRef<HTMLDivElement>(null);
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
        if (cardsRef.current) {
            const cards = cardsRef.current.querySelectorAll(
                `.${styles.configCard}`,
            );
            cards.forEach((card) => {
                (card as HTMLElement).style.opacity = "0";
                (card as HTMLElement).style.transform =
                    "translateY(50px) scale(0.95)";
            });
            const typeValues = cardsRef.current.querySelectorAll(
                `.${styles.typeValue}`,
            );
            typeValues.forEach((val) => {
                (val as HTMLElement).style.opacity = "0";
                (val as HTMLElement).style.transform = "translateX(-20px)";
            });
            const defaultResults = cardsRef.current.querySelectorAll(
                `.${styles.defaultResult}`,
            );
            defaultResults.forEach((result) => {
                (result as HTMLElement).style.opacity = "0";
                (result as HTMLElement).style.transform = "translateX(-20px)";
                result.classList.remove(styles.animated);
            });
        }
        if (explanationRef.current) {
            explanationRef.current.style.opacity = "0";
            explanationRef.current.style.transform = "translateY(20px)";
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

        // Config cards
        addTimeout(() => {
            if (cardsRef.current) {
                const cards = cardsRef.current.querySelectorAll(
                    `.${styles.configCard}`,
                );
                animate(cards, {
                    opacity: 1,
                    translateY: 0,
                    scale: 1,
                    duration: 800,
                    delay: stagger(200),
                    ease: "outBack",
                });
            }
        }, 400);

        // Type values slide in
        addTimeout(() => {
            if (cardsRef.current) {
                const typeValues = cardsRef.current.querySelectorAll(
                    `.${styles.typeValue}`,
                );
                animate(typeValues, {
                    opacity: 1,
                    translateX: 0,
                    duration: 600,
                    delay: stagger(200),
                    ease: "outExpo",
                });
            }
        }, 1000);

        // Default results slide in
        addTimeout(() => {
            if (cardsRef.current) {
                const defaultResults = cardsRef.current.querySelectorAll(
                    `.${styles.defaultResult}`,
                );
                animate(defaultResults, {
                    opacity: 1,
                    translateX: 0,
                    duration: 600,
                    delay: stagger(200),
                    ease: "outExpo",
                });
            }
        }, 1400);

        // Animate default results with glow
        addTimeout(() => {
            if (cardsRef.current) {
                const defaultResults = cardsRef.current.querySelectorAll(
                    `.${styles.defaultResult}`,
                );
                defaultResults.forEach((result, i) => {
                    setTimeout(() => {
                        result.classList.add(styles.animated);
                    }, i * 200);
                });
            }
        }, 1800);

        // Explanation
        addTimeout(() => {
            animate(explanationRef.current, {
                opacity: 1,
                translateY: 0,
                duration: 600,
                ease: "outExpo",
            });
        }, 2200);

        // Complete
        addTimeout(() => {
            if (onComplete) onComplete();
        }, 3000);

        return () => clearAllTimeouts();
    }, [isPlaying, onComplete, clearAllTimeouts, resetAnimation]);

    const configs = [
        {
            name: "per_km_rate",
            type: '{ "type": "number" }',
            default: "10",
        },
        {
            name: "surge_factor",
            type: '{ "type": "number", "minimum": 1.0 }',
            default: "1.0",
        },
        {
            name: "payment_url",
            type: '{ "type": "string", "pattern": "^https://" }',
            default: '"https://pay.juspay.io"',
        },
    ];

    return (
        <div className={styles.slide}>
            <h2 ref={titleRef} className={styles.title}>
                Default Configs
            </h2>
            <p ref={subtitleRef} className={styles.subtitle}>
                Configuration keys with type safety and default values
            </p>

            <div ref={cardsRef} className={styles.configCards}>
                {configs.map((config, index) => (
                    <div
                        key={index}
                        className={styles.configCard}
                        data-config={index + 1}
                    >
                        <div className={styles.configHeader}>
                            <span className={styles.configName}>
                                {config.name}
                            </span>
                        </div>
                        <div className={styles.configBody}>
                            <div className={styles.configAttribute}>
                                <span className={styles.attrLabel}>type</span>
                                <div
                                    className={`${styles.attrValue} ${styles.typeValue}`}
                                >
                                    <code>{config.type}</code>
                                </div>
                            </div>
                            <div className={styles.configAttribute}>
                                <span className={styles.attrLabel}>
                                    default value
                                </span>
                                <div className={styles.attrValue}>
                                    <span className={styles.defaultResult}>
                                        {config.default}
                                    </span>
                                </div>
                            </div>
                        </div>
                    </div>
                ))}
            </div>

            <div ref={explanationRef} className={styles.explanation}>
                <div className={styles.explanationIcon}>
                    <svg
                        viewBox="0 0 24 24"
                        fill="none"
                        stroke="currentColor"
                        strokeWidth="2"
                    >
                        <path d="M9 12l2 2 4-4m5.618-4.016A11.955 11.955 0 0112 2.944a11.955 11.955 0 01-8.618 3.04A12.02 12.02 0 003 9c0 5.591 3.824 10.29 9 11.622 5.176-1.332 9-6.03 9-11.622 0-1.042-.133-2.052-.382-3.016z" />
                    </svg>
                </div>
                <p>
                    Type schema validates every config change before it goes
                    live
                </p>
            </div>
        </div>
    );
}
