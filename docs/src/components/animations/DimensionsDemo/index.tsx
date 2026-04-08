import React, { useEffect, useRef, useCallback, useState } from "react";
import { animate, stagger } from "animejs";
import { useAnimationSlide } from "@site/src/hooks/useAnimationSlide";
import styles from "./styles.module.css";

export default function DimensionsDemo() {
    const { isPlaying, onComplete } = useAnimationSlide();
    const titleRef = useRef<HTMLHeadingElement>(null);
    const subtitleRef = useRef<HTMLParagraphElement>(null);
    const phase1Ref = useRef<HTMLDivElement>(null);
    const phase2Ref = useRef<HTMLDivElement>(null);
    const axesRef = useRef<HTMLDivElement>(null);
    const dimensionLabelsRef = useRef<HTMLDivElement>(null);
    const dimensionValuesRef = useRef<HTMLDivElement>(null);
    const exampleRef = useRef<HTMLDivElement>(null);
    const cardsRef = useRef<HTMLDivElement>(null);
    const schemaValuesRef = useRef<HTMLDivElement>(null);
    const weightValuesRef = useRef<HTMLDivElement>(null);
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
        if (phase1Ref.current) {
            phase1Ref.current.style.opacity = "1";
            phase1Ref.current.style.display = "flex";
        }
        if (phase2Ref.current) {
            phase2Ref.current.style.opacity = "0";
            phase2Ref.current.style.display = "none";
        }
        if (axesRef.current) {
            const axes = axesRef.current.querySelectorAll(`.${styles.axis}`);
            axes.forEach((axis) => {
                (axis as HTMLElement).style.opacity = "0";
                (axis as HTMLElement).style.transform = "scaleY(0)";
            });
        }
        if (dimensionLabelsRef.current) {
            const labels = dimensionLabelsRef.current.querySelectorAll(
                `.${styles.label}`,
            );
            labels.forEach((label) => {
                (label as HTMLElement).style.opacity = "0";
                (label as HTMLElement).style.transform = "translateY(20px)";
            });
        }
        if (dimensionValuesRef.current) {
            const values = dimensionValuesRef.current.querySelectorAll(
                `.${styles.valueItem}`,
            );
            values.forEach((val) => {
                (val as HTMLElement).style.opacity = "0";
                (val as HTMLElement).style.transform = "translateX(-15px)";
            });
        }
        if (exampleRef.current) {
            exampleRef.current.style.opacity = "0";
            exampleRef.current.style.transform = "translateY(20px)";
        }
        if (cardsRef.current) {
            const cards = cardsRef.current.querySelectorAll(`.${styles.card}`);
            cards.forEach((card) => {
                (card as HTMLElement).style.opacity = "0";
                (card as HTMLElement).style.transform =
                    "translateY(50px) scale(0.95)";
            });
        }
        if (schemaValuesRef.current) {
            const schemas = schemaValuesRef.current.querySelectorAll(
                `.${styles.schemaValue}`,
            );
            schemas.forEach((schema) => {
                (schema as HTMLElement).style.opacity = "0";
                (schema as HTMLElement).style.transform = "translateX(-20px)";
            });
        }
        if (weightValuesRef.current) {
            const weights = weightValuesRef.current.querySelectorAll(
                `.${styles.weightValue}`,
            );
            weights.forEach((weight) => {
                (weight as HTMLElement).style.opacity = "0";
                (weight as HTMLElement).style.transform = "translateX(-20px)";
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

        // ===== PHASE 1 ANIMATION =====

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

        // Axes grow up
        addTimeout(() => {
            if (axesRef.current) {
                const axes = axesRef.current.querySelectorAll(
                    `.${styles.dimensionAxis}`,
                );
                animate(axes, {
                    opacity: 1,
                    scaleY: 1,
                    duration: 800,
                    delay: stagger(150),
                    ease: "outExpo",
                });
            }
        }, 400);

        // Dimension labels appear
        addTimeout(() => {
            if (dimensionLabelsRef.current) {
                const labels = dimensionLabelsRef.current.querySelectorAll(
                    `.${styles.label}`,
                );
                animate(labels, {
                    opacity: 1,
                    translateY: 0,
                    duration: 600,
                    delay: stagger(150),
                    ease: "outExpo",
                });
            }
        }, 800);

        // Dimension values stagger in
        addTimeout(() => {
            if (dimensionValuesRef.current) {
                const values = dimensionValuesRef.current.querySelectorAll(
                    `.${styles.valueItem}`,
                );
                animate(values, {
                    opacity: 1,
                    translateX: 0,
                    duration: 500,
                    delay: stagger(80),
                    ease: "outExpo",
                });
            }
        }, 1200);

        // Example code block appears
        addTimeout(() => {
            animate(exampleRef.current, {
                opacity: 1,
                translateY: 0,
                duration: 600,
                ease: "outExpo",
            });
        }, 2000);

        // ===== TRANSITION: Phase 1 → Phase 2 =====

        // Phase 1 fades out and collapses
        addTimeout(() => {
            if (phase1Ref.current) {
                animate(phase1Ref.current, {
                    opacity: 0,
                    duration: 500,
                    ease: "inOutQuad",
                    onComplete: () => {
                        if (phase1Ref.current) {
                            phase1Ref.current.style.display = "none";
                        }
                    },
                });
            }
        }, 3500);

        // Phase 2 container appears
        addTimeout(() => {
            if (phase2Ref.current) {
                phase2Ref.current.style.display = "flex";
                animate(phase2Ref.current, {
                    opacity: 1,
                    duration: 500,
                    ease: "outQuad",
                });
            }
        }, 4000);

        // Cards appear one by one
        addTimeout(() => {
            if (cardsRef.current) {
                const cards = cardsRef.current.querySelectorAll(
                    `.${styles.card}`,
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
        }, 4500);

        // Schema values slide in
        addTimeout(() => {
            if (schemaValuesRef.current) {
                const schemas = schemaValuesRef.current.querySelectorAll(
                    `.${styles.schemaValue}`,
                );
                animate(schemas, {
                    opacity: 1,
                    translateX: 0,
                    duration: 600,
                    delay: stagger(200),
                    ease: "outExpo",
                });
            }
        }, 5100);

        // Weight values slide in
        addTimeout(() => {
            if (weightValuesRef.current) {
                const weights = weightValuesRef.current.querySelectorAll(
                    `.${styles.weightValue}`,
                );
                animate(weights, {
                    opacity: 1,
                    translateX: 0,
                    duration: 600,
                    delay: stagger(200),
                    ease: "outExpo",
                });
            }
        }, 5500);

        // Explanation appears
        addTimeout(() => {
            animate(explanationRef.current, {
                opacity: 1,
                translateY: 0,
                duration: 600,
                ease: "outExpo",
            });
        }, 6500);

        // Complete
        addTimeout(() => {
            if (onComplete) onComplete();
        }, 7500);

        return () => clearAllTimeouts();
    }, [isPlaying, onComplete, clearAllTimeouts, resetAnimation]);

    const dimensions = [
        {
            name: "city",
            values: ["bangalore", "mumbai", "delhi"],
            position: 0,
            schema: '{ "type": "string", "enum": ["bangalore", "mumbai", "delhi"] }',
            weight: 1,
            weightFormula: "2⁰",
        },
        {
            name: "vehicle_type",
            values: ["auto", "sedan", "suv"],
            position: 1,
            schema: '{ "type": "string", "enum": ["auto", "sedan", "suv"] }',
            weight: 2,
            weightFormula: "2¹",
        },
        {
            name: "hour_of_day",
            values: ["morning", "peak", "night"],
            position: 2,
            schema: '{ "type": "string", "enum": ["morning", "peak", "night"] }',
            weight: 4,
            weightFormula: "2²",
        },
    ];

    return (
        <div className={styles.slide}>
            <h2 ref={titleRef} className={styles.title}>
                Dimensions
            </h2>
            <p ref={subtitleRef} className={styles.subtitle}>
                Define the axes of your configuration space
            </p>

            {/* Phase 1: Axis Diagram */}
            <div ref={phase1Ref} className={styles.phase1}>
                <div className={styles.dimensionsVisual}>
                    {dimensions.map((dim, index) => (
                        <div
                            key={index}
                            className={styles.dimension}
                            data-dimension={index + 1}
                        >
                            <div
                                ref={axesRef}
                                className={styles.dimensionAxis}
                            ></div>
                            <span className={styles.dimensionLabel}>
                                {dim.name}
                            </span>
                            <div className={styles.dimensionValues}>
                                {dim.values.map((val, vIndex) => (
                                    <span key={vIndex}>{val}</span>
                                ))}
                            </div>
                        </div>
                    ))}
                </div>

                <div ref={exampleRef} className={styles.example}>
                    <code>
                        Context: {"{"} city: "bangalore", vehicle_type: "sedan",
                        hour: "peak" {"}"}
                    </code>
                </div>
            </div>

            {/* Phase 2: Attribute Cards */}
            <div ref={phase2Ref} className={styles.phase2}>
                <p className={styles.phaseSubtitle}>
                    Each dimension has a type schema and a priority weight
                </p>

                <div ref={cardsRef} className={styles.dimensionCards}>
                    {dimensions.map((dim, index) => (
                        <div
                            key={index}
                            className={styles.dimensionCard}
                            data-position={dim.position}
                        >
                            <div className={styles.cardHeader}>
                                <span className={styles.cardPosition}>
                                    Position {dim.position}
                                </span>
                                <span className={styles.cardName}>
                                    {dim.name}
                                </span>
                            </div>
                            <div className={styles.cardBody}>
                                <div className={styles.cardAttribute}>
                                    <span className={styles.attrLabel}>
                                        type
                                    </span>
                                    <div
                                        ref={schemaValuesRef}
                                        className={styles.attrValue}
                                    >
                                        <code>{dim.schema}</code>
                                    </div>
                                </div>
                                <div className={styles.cardAttribute}>
                                    <span className={styles.attrLabel}>
                                        weight
                                    </span>
                                    <div
                                        ref={weightValuesRef}
                                        className={styles.weightValue}
                                    >
                                        <span className={styles.weightFormula}>
                                            {dim.weightFormula}
                                        </span>
                                        <span className={styles.weightEquals}>
                                            =
                                        </span>
                                        <span className={styles.weightResult}>
                                            {dim.weight}
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
                            <circle cx="12" cy="12" r="10" />
                            <path d="M12 16v-4M12 8h.01" />
                        </svg>
                    </div>
                    <p>Higher weight = Higher priority in context resolution</p>
                </div>
            </div>
        </div>
    );
}
