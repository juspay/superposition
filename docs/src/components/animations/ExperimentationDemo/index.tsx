import React, { useEffect, useRef, useCallback, useState } from "react";
import { animate, stagger } from "animejs";
import { useAnimationSlide } from "@site/src/hooks/useAnimationSlide";
import styles from "./styles.module.css";

export default function ExperimentationDemo() {
    const { isPlaying, onComplete } = useAnimationSlide();
    const titleRef = useRef<HTMLHeadingElement>(null);
    const subtitleRef = useRef<HTMLParagraphElement>(null);
    const controlsRef = useRef<HTMLDivElement>(null);
    const legendRef = useRef<HTMLDivElement>(null);
    const gridRef = useRef<HTMLDivElement>(null);
    const distributionRef = useRef<HTMLDivElement>(null);
    const timeoutRefs = useRef<ReturnType<typeof setTimeout>[]>([]);

    const [variantCount, setVariantCount] = useState(1);
    const [traffic, setTraffic] = useState(0);
    const [dotOrder, setDotOrder] = useState<number[]>([]);
    // Track sticky bucket assignments for each dot (permanent memory - once assigned, never forgotten)
    // null = never assigned to experiment, "control" / "variant-N" = bucket assignment
    const dotBucketRef = useRef<(string | null)[]>(Array(100).fill(null));

    const clearAllTimeouts = useCallback(() => {
        timeoutRefs.current.forEach(clearTimeout);
        timeoutRefs.current = [];
    }, []);

    const shuffleArray = useCallback((arr: number[]) => {
        const shuffled = [...arr];
        for (let i = shuffled.length - 1; i > 0; i--) {
            const j = Math.floor(Math.random() * (i + 1));
            [shuffled[i], shuffled[j]] = [shuffled[j], shuffled[i]];
        }
        return shuffled;
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
        if (controlsRef.current) {
            controlsRef.current.style.opacity = "0";
            controlsRef.current.style.transform = "translateY(20px)";
        }
        if (legendRef.current) {
            legendRef.current.style.opacity = "0";
        }
        if (gridRef.current) {
            gridRef.current.style.opacity = "0";
            gridRef.current.style.transform = "scale(0.95)";
            const dots = gridRef.current.querySelectorAll(`.${styles.dot}`);
            dots.forEach((dot) => {
                (dot as HTMLElement).style.opacity = "0";
                (dot as HTMLElement).style.transform = "scale(0)";
            });
        }
        if (distributionRef.current) {
            distributionRef.current.style.opacity = "0";
            distributionRef.current.style.transform = "translateY(20px)";
        }
        // Reset sticky bucket assignments
        dotBucketRef.current = Array(100).fill(null);
    }, [clearAllTimeouts]);

    // Calculate derived values
    const totalGroups = variantCount + 1;
    const maxTraffic = Math.floor(100 / totalGroups);
    const currentTraffic = Math.min(traffic, maxTraffic);
    const controlPercent = currentTraffic;
    const variantPercents = Array(variantCount).fill(currentTraffic);
    const totalExperimentTraffic =
        controlPercent + variantPercents.reduce((a, b) => a + b, 0);
    const productionPercent = 100 - totalExperimentTraffic;

    const updateDistribution = useCallback(() => {
        if (!gridRef.current) return;

        const buckets = dotBucketRef.current;
        const newBuckets = [...buckets];

        // Calculate how many dots we need for each group
        const neededControl = controlPercent;
        const neededVariants = Array(variantCount)
            .fill(0)
            .map(() => currentTraffic);

        // Get dots that have a matching bucket (sticky recall)
        const controlDotsWithBucket: number[] = [];
        const variantDotsWithBucket: number[][] = Array(variantCount)
            .fill(null)
            .map(() => []);

        dotOrder.forEach((dotIndex) => {
            const bucket = buckets[dotIndex];
            if (bucket === "control") {
                controlDotsWithBucket.push(dotIndex);
            } else if (bucket && bucket.startsWith("variant-")) {
                const variantNum = parseInt(bucket.split("-")[1]);
                if (variantNum <= variantCount) {
                    variantDotsWithBucket[variantNum - 1].push(dotIndex);
                }
            }
        });

        // Get dots without any bucket assignment (never been in experiment)
        const unbucketedDots = dotOrder.filter((i) => !buckets[i]);

        // Determine which dots to show for each group
        const displayControl: number[] = [];
        const displayVariants: number[][] = Array(variantCount)
            .fill(null)
            .map(() => []);

        // Fill control slots: first use dots with matching bucket, then new dots
        const controlFromBucket = controlDotsWithBucket.slice(0, neededControl);
        displayControl.push(...controlFromBucket);

        let unbucketedIndex = 0;
        const controlFromNew = neededControl - controlFromBucket.length;
        for (
            let i = 0;
            i < controlFromNew && unbucketedIndex < unbucketedDots.length;
            i++
        ) {
            const dotIndex = unbucketedDots[unbucketedIndex++];
            displayControl.push(dotIndex);
            newBuckets[dotIndex] = "control";
        }

        // Fill variant slots: first use dots with matching bucket, then new dots
        for (let v = 0; v < variantCount; v++) {
            const fromBucket = variantDotsWithBucket[v].slice(
                0,
                neededVariants[v],
            );
            displayVariants[v].push(...fromBucket);

            const fromNew = neededVariants[v] - fromBucket.length;
            for (
                let i = 0;
                i < fromNew && unbucketedIndex < unbucketedDots.length;
                i++
            ) {
                const dotIndex = unbucketedDots[unbucketedIndex++];
                displayVariants[v].push(dotIndex);
                newBuckets[dotIndex] = `variant-${v + 1}`;
            }
        }

        // Update bucket memory
        dotBucketRef.current = newBuckets;

        // Build a map of which dots should display which color
        const displayMap: Map<number, string> = new Map();
        displayControl.forEach((i) => displayMap.set(i, "control"));
        displayVariants.forEach((variantDots, v) => {
            variantDots.forEach((i) => displayMap.set(i, `variant-${v + 1}`));
        });

        // Apply to DOM
        const dots = gridRef.current.querySelectorAll(`.${styles.dot}`);
        dots.forEach((dot, index) => {
            dot.className = styles.dot;
            const color = displayMap.get(index);
            if (color === "control") {
                dot.classList.add(styles.control);
            } else if (color && color.startsWith("variant-")) {
                const variantClass = styles[color as keyof typeof styles];
                dot.classList.add(variantClass || styles.production);
            } else {
                dot.classList.add(styles.production);
            }
        });
    }, [variantCount, currentTraffic, dotOrder, controlPercent]);

    // Initialize styles on mount - ensures elements start in correct state
    useEffect(() => {
        if (titleRef.current) {
            titleRef.current.style.opacity = "0";
            titleRef.current.style.transform = "translateY(40px)";
        }
        if (subtitleRef.current) {
            subtitleRef.current.style.opacity = "0";
            subtitleRef.current.style.transform = "translateY(30px)";
        }
        if (controlsRef.current) {
            controlsRef.current.style.opacity = "0";
            controlsRef.current.style.transform = "translateY(20px)";
        }
        if (legendRef.current) {
            legendRef.current.style.opacity = "0";
        }
        if (gridRef.current) {
            gridRef.current.style.opacity = "0";
            gridRef.current.style.transform = "scale(0.95)";
            const dots = gridRef.current.querySelectorAll(`.${styles.dot}`);
            dots.forEach((dot) => {
                (dot as HTMLElement).style.opacity = "0";
                (dot as HTMLElement).style.transform = "scale(0)";
            });
        }
        if (distributionRef.current) {
            distributionRef.current.style.opacity = "0";
            distributionRef.current.style.transform = "translateY(20px)";
        }
    }, []);

    useEffect(() => {
        setDotOrder(shuffleArray(Array.from({ length: 100 }, (_, i) => i)));
    }, [shuffleArray]);

    useEffect(() => {
        updateDistribution();
    }, [updateDistribution]);

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

        // Controls
        addTimeout(() => {
            animate(controlsRef.current, {
                opacity: 1,
                translateY: 0,
                duration: 600,
                ease: "outExpo",
            });
        }, 400);

        // Legend
        addTimeout(() => {
            animate(legendRef.current, {
                opacity: 1,
                duration: 500,
                ease: "outExpo",
            });
        }, 600);

        // Grid and dots
        addTimeout(() => {
            animate(gridRef.current, {
                opacity: 1,
                scale: 1,
                duration: 600,
                ease: "outExpo",
            });
            if (gridRef.current) {
                const dots = gridRef.current.querySelectorAll(`.${styles.dot}`);
                animate(dots, {
                    opacity: 1,
                    scale: 1,
                    duration: 400,
                    delay: stagger(15),
                    ease: "outBack",
                });
            }
        }, 700);

        // Distribution
        addTimeout(() => {
            animate(distributionRef.current, {
                opacity: 1,
                translateY: 0,
                duration: 600,
                ease: "outExpo",
            });
        }, 1200);

        // Initialize distribution after animation starts
        addTimeout(() => {
            updateDistribution();
        }, 800);

        // Complete
        addTimeout(() => {
            if (onComplete) onComplete();
        }, 2000);

        return () => clearAllTimeouts();
    }, [
        isPlaying,
        onComplete,
        clearAllTimeouts,
        resetAnimation,
        updateDistribution,
    ]);

    const handleVariantChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        let val = parseInt(e.target.value) || 1;
        val = Math.max(1, Math.min(4, val));
        setVariantCount(val);
        // Reset traffic and dot assignments when variant count changes
        setTraffic(0);
        dotBucketRef.current = Array(100).fill(null);
    };

    const handleTrafficChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        const val = parseInt(e.target.value) || 0;
        setTraffic(Math.max(0, Math.min(maxTraffic, val)));
    };

    return (
        <div className={styles.slide}>
            <h2 ref={titleRef} className={styles.title}>
                Experimentation
            </h2>
            <p ref={subtitleRef} className={styles.subtitle}>
                A/B test configuration changes before full rollout
            </p>

            <div className={styles.visual}>
                {/* Controls */}
                <div ref={controlsRef} className={styles.controls}>
                    <div className={styles.controlGroup}>
                        <label htmlFor="variant-count">Variants</label>
                        <input
                            type="number"
                            id="variant-count"
                            min="1"
                            max="4"
                            value={variantCount}
                            onChange={handleVariantChange}
                        />
                    </div>
                    <div className={styles.controlGroup}>
                        <label htmlFor="traffic-slider">
                            Experiment Traffic:{" "}
                            <span id="traffic-percent">{currentTraffic}%</span>
                        </label>
                        <input
                            type="range"
                            id="traffic-slider"
                            min="0"
                            max={maxTraffic}
                            value={currentTraffic}
                            onChange={handleTrafficChange}
                        />
                    </div>
                </div>

                {/* Legend */}
                <div ref={legendRef} className={styles.legend}>
                    <div className={styles.legendItem}>
                        <span
                            className={`${styles.legendDot} ${styles.production}`}
                        ></span>
                        <span>Production</span>
                    </div>
                    <div className={styles.legendItem}>
                        <span
                            className={`${styles.legendDot} ${styles.control}`}
                        ></span>
                        <span>Control</span>
                    </div>
                    {[1, 2, 3, 4].map((i) => (
                        <div
                            key={i}
                            className={`${styles.legendItem} ${styles.legendVariant}`}
                            data-variant={i}
                            style={{
                                display: i <= variantCount ? "flex" : "none",
                            }}
                        >
                            <span
                                className={`${styles.legendDot} ${styles[`variant-${i}` as keyof typeof styles]}`}
                            ></span>
                            <span>Variant {i}</span>
                        </div>
                    ))}
                </div>

                {/* Dots Grid */}
                <div ref={gridRef} className={styles.dotsGrid}>
                    {Array.from({ length: 100 }, (_, i) => (
                        <div
                            key={i}
                            className={`${styles.dot} ${styles.production}`}
                        ></div>
                    ))}
                </div>

                {/* Distribution */}
                <div ref={distributionRef} className={styles.distribution}>
                    <div className={`${styles.distItem} ${styles.production}`}>
                        <span className={styles.distLabel}>Production</span>
                        <span className={styles.distValue}>
                            {productionPercent}%
                        </span>
                    </div>
                    <div className={`${styles.distItem} ${styles.control}`}>
                        <span className={styles.distLabel}>Control</span>
                        <span className={styles.distValue}>
                            {controlPercent}%
                        </span>
                    </div>
                    {[1, 2, 3, 4].map((i) => (
                        <div
                            key={i}
                            className={`${styles.distItem} ${styles.variant}`}
                            data-variant={i}
                            style={{
                                display: i <= variantCount ? "flex" : "none",
                            }}
                        >
                            <span className={styles.distLabel}>
                                Variant {i}
                            </span>
                            <span className={styles.distValue}>
                                {currentTraffic}%
                            </span>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
}
