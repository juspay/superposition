import React, { useEffect, useRef, useCallback } from "react";
import { animate } from "animejs";
import { useAnimationSlide } from "@site/src/hooks/useAnimationSlide";
import styles from "./styles.module.css";

export default function CSSAnalogy() {
    const { isPlaying, onComplete } = useAnimationSlide();
    const titleRef = useRef<HTMLHeadingElement>(null);
    const subtitleRef = useRef<HTMLParagraphElement>(null);
    const cssPanelRef = useRef<HTMLDivElement>(null);
    const cacPanelRef = useRef<HTMLDivElement>(null);
    const connectorRef = useRef<HTMLDivElement>(null);
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
        if (cssPanelRef.current) {
            cssPanelRef.current.style.opacity = "0";
            cssPanelRef.current.style.transform = "translateX(-50px)";
        }
        if (cacPanelRef.current) {
            cacPanelRef.current.style.opacity = "0";
            cacPanelRef.current.style.transform = "translateX(50px)";
        }
        if (connectorRef.current) {
            connectorRef.current.style.opacity = "0";
            connectorRef.current.style.transform = "scale(0)";
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

        // CSS Panel
        addTimeout(() => {
            animate(cssPanelRef.current, {
                opacity: 1,
                translateX: 0,
                duration: 800,
                ease: "outExpo",
            });
        }, 400);

        // CAC Panel
        addTimeout(() => {
            animate(cacPanelRef.current, {
                opacity: 1,
                translateX: 0,
                duration: 800,
                ease: "outExpo",
            });
        }, 600);

        // Connector
        addTimeout(() => {
            animate(connectorRef.current, {
                opacity: 1,
                scale: 1,
                duration: 600,
                ease: "outBack",
            });
        }, 1000);

        // Complete
        addTimeout(() => {
            if (onComplete) onComplete();
        }, 2000);

        return () => clearAllTimeouts();
    }, [isPlaying, onComplete, clearAllTimeouts, resetAnimation]);

    return (
        <div className={styles.slide}>
            <h2 ref={titleRef} className={styles.title}>
                Like CSS, But For Your Backend
            </h2>
            <p ref={subtitleRef} className={styles.subtitle}>
                Familiar specificity rules for configuration
            </p>

            <div className={styles.analogyContainer}>
                <div ref={cssPanelRef} className={styles.analogyPanel}>
                    <h3>CSS Cascade</h3>
                    <pre className={styles.codeBlock}>
                        <code>
                            <span className={styles.codeSelector}>*</span>{" "}
                            {"{"}{" "}
                            <span className={styles.codeProp}>color</span>:
                            black; {"}"}
                            {"\n\n"}
                            <span className={styles.codeSelector}>.button</span>{" "}
                            {"{"}{" "}
                            <span className={styles.codeProp}>color</span>:
                            blue; {"}"}
                            {"\n\n"}
                            <span className={styles.codeSelector}>#submit</span>{" "}
                            {"{"}{" "}
                            <span className={styles.codeProp}>color</span>:
                            green; {"}"}
                        </code>
                    </pre>
                    <p className={styles.analogyNote}>
                        More specific selector wins
                    </p>
                </div>

                <div ref={connectorRef} className={styles.connector}>
                    <span>≈</span>
                </div>

                <div ref={cacPanelRef} className={styles.analogyPanel}>
                    <h3>CAC Override</h3>
                    <pre className={styles.codeBlock}>
                        <code>
                            <span className={styles.codeSelector}>default</span>:
                            surge ={" "}
                            <span className={styles.codeValue}>1.0</span>
                            {"\n\n"}
                            <span className={styles.codeSelector}>
                                city=blr
                            </span>
                            : surge ={" "}
                            <span className={styles.codeValue}>1.2</span>
                            {"\n\n"}
                            <span className={styles.codeSelector}>
                                city=blr&amp;peak
                            </span>
                            : surge ={" "}
                            <span className={styles.codeValue}>1.8</span>
                        </code>
                    </pre>
                    <p className={styles.analogyNote}>
                        More specific context wins
                    </p>
                </div>
            </div>
        </div>
    );
}
