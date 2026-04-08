import React, { useEffect, useRef, useCallback, useState } from "react";
import { animate } from "animejs";
import { useAnimationSlide } from "@site/src/hooks/useAnimationSlide";
import styles from "./styles.module.css";

interface Context {
    city: string;
    vehicle: string;
    cityWeight: number;
    vehicleWeight: number;
    totalWeight: number;
    showRejected: boolean;
    rejectedCity: string | null;
    rejectedCityVals?: { per_km_rate: string; surge_factor: string };
    rejectedCombo: { city: string; vehicle: string } | null;
    rejectedComboVals?: { per_km_rate: string; surge_factor: string };
    cityVals: { per_km_rate: string; surge_factor: string };
    comboVals: {
        per_km_rate: string;
        surge_factor: string;
        timeout_ms: string;
    };
    final: {
        per_km_rate: string;
        surge_factor: string;
        timeout_ms: string;
        weight: string;
    };
}

const contexts: Context[] = [
    {
        city: "bangalore",
        vehicle: "auto",
        cityWeight: 1,
        vehicleWeight: 2,
        totalWeight: 3,
        showRejected: false,
        rejectedCity: null,
        rejectedCombo: null,
        cityVals: { per_km_rate: "12", surge_factor: "1.2" },
        comboVals: {
            per_km_rate: "14",
            surge_factor: "1.5",
            timeout_ms: "3000",
        },
        final: {
            per_km_rate: "14",
            surge_factor: "1.5",
            timeout_ms: "3000",
            weight: "3",
        },
    },
    {
        city: "bangalore",
        vehicle: "sedan",
        cityWeight: 1,
        vehicleWeight: 2,
        totalWeight: 3,
        showRejected: true,
        rejectedCity: "mumbai",
        rejectedCityVals: { per_km_rate: "13", surge_factor: "1.1" },
        rejectedCombo: { city: "mumbai", vehicle: "auto" },
        rejectedComboVals: { per_km_rate: "15", surge_factor: "1.3" },
        cityVals: { per_km_rate: "12", surge_factor: "1.2" },
        comboVals: {
            per_km_rate: "15",
            surge_factor: "1.4",
            timeout_ms: "4000",
        },
        final: {
            per_km_rate: "15",
            surge_factor: "1.4",
            timeout_ms: "4000",
            weight: "3",
        },
    },
    {
        city: "mumbai",
        vehicle: "auto",
        cityWeight: 1,
        vehicleWeight: 2,
        totalWeight: 3,
        showRejected: false,
        rejectedCity: null,
        rejectedCombo: null,
        cityVals: { per_km_rate: "13", surge_factor: "1.1" },
        comboVals: {
            per_km_rate: "14",
            surge_factor: "1.3",
            timeout_ms: "3000",
        },
        final: {
            per_km_rate: "14",
            surge_factor: "1.3",
            timeout_ms: "3000",
            weight: "3",
        },
    },
    {
        city: "mumbai",
        vehicle: "sedan",
        cityWeight: 1,
        vehicleWeight: 2,
        totalWeight: 3,
        showRejected: true,
        rejectedCity: "bangalore",
        rejectedCityVals: { per_km_rate: "12", surge_factor: "1.2" },
        rejectedCombo: { city: "bangalore", vehicle: "auto" },
        rejectedComboVals: { per_km_rate: "14", surge_factor: "1.5" },
        cityVals: { per_km_rate: "13", surge_factor: "1.1" },
        comboVals: {
            per_km_rate: "16",
            surge_factor: "1.6",
            timeout_ms: "3500",
        },
        final: {
            per_km_rate: "16",
            surge_factor: "1.6",
            timeout_ms: "3500",
            weight: "3",
        },
    },
];

export default function ContextOverrides() {
    const { isPlaying, onComplete } = useAnimationSlide();
    const titleRef = useRef<HTMLHeadingElement>(null);
    const queueRef = useRef<HTMLDivElement>(null);
    const resolutionRef = useRef<HTMLDivElement>(null);
    const layerBaseRef = useRef<HTMLDivElement>(null);
    const layerCityRef = useRef<HTMLDivElement>(null);
    const layerRejectedRef = useRef<HTMLDivElement>(null);
    const layerComboRef = useRef<HTMLDivElement>(null);
    const layerRejected2Ref = useRef<HTMLDivElement>(null);
    const timeoutRefs = useRef<ReturnType<typeof setTimeout>[]>([]);
    const loopIntervalRef = useRef<ReturnType<typeof setTimeout> | null>(null);
    const [isPaused, setIsPaused] = useState(false);
    const [currentIndex, setCurrentIndex] = useState(0);
    const [ctxState, setCtxState] = useState<Context>(contexts[0]);
    const [resValues, setResValues] = useState({
        per_km_rate: "-",
        surge: "-",
        timeout: "-",
        weight: "-",
    });
    const [layerStates, setLayerStates] = useState({
        base: { visible: false, evaluating: false, sweeping: false },
        city: { visible: false, evaluating: false, sweeping: false },
        rejected: { visible: false, evaluating: false, sweeping: false },
        combo: { visible: false, evaluating: false, sweeping: false },
        rejected2: { visible: false, evaluating: false, sweeping: false },
    });

    const clearAllTimeouts = useCallback(() => {
        timeoutRefs.current.forEach(clearTimeout);
        timeoutRefs.current = [];
        if (loopIntervalRef.current) {
            clearTimeout(loopIntervalRef.current);
            loopIntervalRef.current = null;
        }
    }, []);

    const resetAnimation = useCallback(() => {
        clearAllTimeouts();
        setIsPaused(false);
        setCurrentIndex(0);
        setCtxState(contexts[0]);
        setResValues({
            per_km_rate: "-",
            surge: "-",
            timeout: "-",
            weight: "-",
        });
        setLayerStates({
            base: { visible: false, evaluating: false, sweeping: false },
            city: { visible: false, evaluating: false, sweeping: false },
            rejected: { visible: false, evaluating: false, sweeping: false },
            combo: { visible: false, evaluating: false, sweeping: false },
            rejected2: { visible: false, evaluating: false, sweeping: false },
        });

        if (titleRef.current) {
            titleRef.current.style.opacity = "0";
            titleRef.current.style.transform = "translateY(30px)";
        }
        if (queueRef.current) {
            const items = queueRef.current.querySelectorAll(
                `.${styles.ctxItem}`,
            );
            items.forEach((item) =>
                item.classList.remove(
                    styles.visible,
                    styles.active,
                    styles.done,
                ),
            );
        }
    }, [clearAllTimeouts]);

    const addTimeout = useCallback(
        (fn: () => void, delay: number) => {
            if (isPaused) return null;
            const id = setTimeout(fn, delay);
            timeoutRefs.current.push(id);
            return id;
        },
        [isPaused],
    );

    const runContextAnimation = useCallback(
        (index: number) => {
            const ctx = contexts[index];
            setCtxState(ctx);
            setCurrentIndex(index);

            const CYCLE_TIME = ctx.showRejected ? 14000 : 10000;

            // Update queue items
            if (queueRef.current) {
                const items = queueRef.current.querySelectorAll(
                    `.${styles.ctxItem}`,
                );
                items.forEach((item, i) => {
                    item.classList.remove(styles.active, styles.done);
                    if (i < index) {
                        item.classList.add(styles.done);
                    }
                });
                items[index]?.classList.add(styles.active);
            }

            // Reset results
            setResValues({
                per_km_rate: "-",
                surge: "-",
                timeout: "-",
                weight: "-",
            });

            // Reset all layers
            setLayerStates({
                base: { visible: false, evaluating: false, sweeping: false },
                city: { visible: false, evaluating: false, sweeping: false },
                rejected: {
                    visible: false,
                    evaluating: false,
                    sweeping: false,
                },
                combo: { visible: false, evaluating: false, sweeping: false },
                rejected2: {
                    visible: false,
                    evaluating: false,
                    sweeping: false,
                },
            });

            let rejectedDelay = 0;

            // === Animation sequence ===

            // Base layer - starts at 400ms
            addTimeout(() => {
                setLayerStates((prev) => ({
                    ...prev,
                    base: { visible: true, evaluating: true, sweeping: false },
                }));
                addTimeout(() => {
                    setLayerStates((prev) => ({
                        ...prev,
                        base: { ...prev.base, sweeping: true },
                    }));
                }, 150);
            }, 400);

            // City layer - starts at 2800ms
            addTimeout(() => {
                setLayerStates((prev) => ({
                    ...prev,
                    city: { visible: true, evaluating: true, sweeping: false },
                }));
                addTimeout(() => {
                    setLayerStates((prev) => ({
                        ...prev,
                        city: { ...prev.city, sweeping: true },
                    }));
                }, 150);
            }, 2800);

            addTimeout(() => {
                setLayerStates((prev) => ({
                    ...prev,
                    base: { ...prev.base, evaluating: false },
                }));
            }, 2400);

            addTimeout(() => {
                setLayerStates((prev) => ({
                    ...prev,
                    city: { ...prev.city, evaluating: false },
                }));
            }, 5000);

            // Show rejected city layer if applicable
            if (ctx.showRejected && ctx.rejectedCity) {
                rejectedDelay = 2500;

                addTimeout(() => {
                    setLayerStates((prev) => ({
                        ...prev,
                        rejected: {
                            visible: true,
                            evaluating: true,
                            sweeping: false,
                        },
                    }));
                    addTimeout(() => {
                        setLayerStates((prev) => ({
                            ...prev,
                            rejected: { ...prev.rejected, sweeping: true },
                        }));
                    }, 150);
                }, 5400);

                addTimeout(() => {
                    setLayerStates((prev) => ({
                        ...prev,
                        rejected: { ...prev.rejected, evaluating: false },
                    }));
                }, 7200);
            }

            // Combo layer - starts at 5400 + rejectedDelay
            addTimeout(() => {
                setLayerStates((prev) => ({
                    ...prev,
                    combo: { visible: true, evaluating: true, sweeping: false },
                }));
                addTimeout(() => {
                    setLayerStates((prev) => ({
                        ...prev,
                        combo: { ...prev.combo, sweeping: true },
                    }));
                }, 150);
            }, 5400 + rejectedDelay);

            addTimeout(() => {
                setLayerStates((prev) => ({
                    ...prev,
                    combo: { ...prev.combo, evaluating: false },
                }));
            }, 7600 + rejectedDelay);

            // Show rejected combo layer if applicable
            if (ctx.showRejected && ctx.rejectedCombo) {
                addTimeout(() => {
                    setLayerStates((prev) => ({
                        ...prev,
                        rejected2: {
                            visible: true,
                            evaluating: true,
                            sweeping: false,
                        },
                    }));
                    addTimeout(() => {
                        setLayerStates((prev) => ({
                            ...prev,
                            rejected2: { ...prev.rejected2, sweeping: true },
                        }));
                    }, 150);
                }, 8000 + rejectedDelay);

                addTimeout(() => {
                    setLayerStates((prev) => ({
                        ...prev,
                        rejected2: { ...prev.rejected2, evaluating: false },
                    }));
                }, 9600 + rejectedDelay);
            }

            // Show final result
            const finalTime = ctx.showRejected ? 10000 + rejectedDelay : 8000;
            addTimeout(() => {
                setResValues((prev) => ({ ...prev, weight: ctx.final.weight }));
            }, finalTime);

            addTimeout(() => {
                setResValues((prev) => ({
                    ...prev,
                    per_km_rate: ctx.final.per_km_rate,
                }));
            }, finalTime + 300);

            addTimeout(() => {
                setResValues((prev) => ({
                    ...prev,
                    surge: ctx.final.surge_factor,
                }));
            }, finalTime + 600);

            addTimeout(() => {
                setResValues((prev) => ({
                    ...prev,
                    timeout: ctx.final.timeout_ms,
                }));
            }, finalTime + 900);

            // Mark done and schedule next
            const doneTime = ctx.showRejected ? 12000 + rejectedDelay : 9500;
            addTimeout(() => {
                if (queueRef.current) {
                    const items = queueRef.current.querySelectorAll(
                        `.${styles.ctxItem}`,
                    );
                    items[index]?.classList.remove(styles.active);
                    items[index]?.classList.add(styles.done);
                }
            }, doneTime);

            // Loop to next
            loopIntervalRef.current = setTimeout(() => {
                if (!isPaused) {
                    runContextAnimation((index + 1) % contexts.length);
                }
            }, CYCLE_TIME);
        },
        [addTimeout, isPaused],
    );

    useEffect(() => {
        if (!isPlaying) {
            resetAnimation();
            return;
        }

        // Title animation
        animate(titleRef.current, {
            opacity: 1,
            translateY: 0,
            duration: 800,
            ease: "outExpo",
        });

        // Show queue items
        if (queueRef.current) {
            const items = queueRef.current.querySelectorAll(
                `.${styles.ctxItem}`,
            );
            items.forEach((item, i) => {
                setTimeout(
                    () => item.classList.add(styles.visible),
                    400 + i * 150,
                );
            });
        }

        // Start loop
        setTimeout(() => runContextAnimation(0), 1500);

        return () => clearAllTimeouts();
    }, [isPlaying, resetAnimation, clearAllTimeouts, runContextAnimation]);

    const togglePause = () => {
        setIsPaused(!isPaused);
        if (!isPaused) {
            clearAllTimeouts();
        } else {
            runContextAnimation(currentIndex);
        }
    };

    const getLayerClasses = (
        layerType: "base" | "city" | "rejected" | "combo" | "rejected2",
    ) => {
        const state = layerStates[layerType];
        const classes = [styles.ctxLayer];

        if (layerType === "base") classes.push(styles.layerBase);
        if (layerType === "city") classes.push(styles.layerCity);
        if (layerType === "rejected") classes.push(styles.layerRejected);
        if (layerType === "combo") classes.push(styles.layerCombo);
        if (layerType === "rejected2") classes.push(styles.layerRejected2);

        if (state.visible) classes.push(styles.visible);
        if (state.evaluating) classes.push(styles.evaluating);
        if (state.sweeping) classes.push(styles.sweeping);

        return classes.join(" ");
    };

    return (
        <div className={styles.slide}>
            <h2 ref={titleRef} className={styles.title}>
                Context & Overrides
            </h2>

            <div className={styles.layout}>
                {/* Left: Context Queue */}
                <div ref={queueRef} className={styles.queue}>
                    <div className={styles.queueHeader}>
                        Request Queue
                        <button
                            className={styles.pauseBtn}
                            onClick={togglePause}
                            title="Pause/Play"
                        >
                            {isPaused ? (
                                <svg viewBox="0 0 24 24" fill="currentColor">
                                    <polygon points="5,3 19,12 5,21" />
                                </svg>
                            ) : (
                                <svg viewBox="0 0 24 24" fill="currentColor">
                                    <rect x="6" y="4" width="4" height="16" />
                                    <rect x="14" y="4" width="4" height="16" />
                                </svg>
                            )}
                        </button>
                    </div>
                    <div className={styles.queueItems}>
                        {contexts.map((ctx, index) => (
                            <div
                                key={index}
                                className={styles.ctxItem}
                                data-context={index}
                            >
                                <span className={styles.ctxBadge}>
                                    {index + 1}
                                </span>
                                <div className={styles.ctxTags}>
                                    <span className={styles.ctxTag}>
                                        {ctx.city}
                                    </span>
                                    <span className={styles.ctxTag}>
                                        {ctx.vehicle}
                                    </span>
                                </div>
                                <span className={styles.ctxStatus}></span>
                            </div>
                        ))}
                    </div>
                </div>

                {/* Right: Config Resolution */}
                <div ref={resolutionRef} className={styles.resolution}>
                    <div className={styles.evalHeader}>
                        <span>Evaluating:</span>
                        <span className={styles.evalCity}>{ctxState.city}</span>
                        <span className={styles.evalWeight}>
                            (w={ctxState.cityWeight})
                        </span>
                        <span className={styles.evalSep}>×</span>
                        <span className={styles.evalVehicle}>
                            {ctxState.vehicle}
                        </span>
                        <span className={styles.evalWeight}>
                            (w={ctxState.vehicleWeight})
                        </span>
                        <span className={styles.evalTotal}>
                            = {ctxState.totalWeight}
                        </span>
                    </div>

                    <div className={styles.layers}>
                        {/* Base Layer */}
                        <div
                            ref={layerBaseRef}
                            className={getLayerClasses("base")}
                            data-layer="default"
                            data-weight="0"
                        >
                            <div className={styles.layerHead}>
                                <span className={styles.layerDot}></span>
                                <span>Base</span>
                                <span className={styles.layerWeight}>
                                    weight: 0
                                </span>
                            </div>
                            <div className={styles.layerRows}>
                                <div
                                    className={styles.ctxRow}
                                    data-key="per_km_rate"
                                >
                                    <span>per_km_rate</span>
                                    <span className={styles.ctxVal}>10</span>
                                </div>
                                <div
                                    className={styles.ctxRow}
                                    data-key="surge_factor"
                                >
                                    <span>surge_factor</span>
                                    <span className={styles.ctxVal}>1.0</span>
                                </div>
                                <div
                                    className={styles.ctxRow}
                                    data-key="timeout_ms"
                                >
                                    <span>timeout_ms</span>
                                    <span className={styles.ctxVal}>5000</span>
                                </div>
                            </div>
                        </div>

                        {/* City Layer */}
                        <div
                            ref={layerCityRef}
                            className={getLayerClasses("city")}
                            data-layer="city"
                            data-weight="1"
                        >
                            <div className={styles.layerHead}>
                                <span className={styles.layerDot}></span>
                                <span>
                                    City:{" "}
                                    <span className={styles.matchCity}>
                                        {ctxState.city}
                                    </span>
                                </span>
                                <span className={styles.layerWeight}>
                                    weight: 1
                                </span>
                            </div>
                            <div className={styles.layerRows}>
                                <div
                                    className={styles.ctxRow}
                                    data-key="per_km_rate"
                                >
                                    <span>per_km_rate</span>
                                    <span
                                        className={`${styles.ctxVal} ${styles.override}`}
                                    >
                                        {ctxState.cityVals.per_km_rate}
                                    </span>
                                    <span className={styles.ctxArrow}>→</span>
                                </div>
                                <div
                                    className={styles.ctxRow}
                                    data-key="surge_factor"
                                >
                                    <span>surge_factor</span>
                                    <span
                                        className={`${styles.ctxVal} ${styles.override}`}
                                    >
                                        {ctxState.cityVals.surge_factor}
                                    </span>
                                    <span className={styles.ctxArrow}>→</span>
                                </div>
                                <div
                                    className={styles.ctxRow}
                                    data-key="timeout_ms"
                                >
                                    <span>timeout_ms</span>
                                    <span className={styles.ctxVal}>5000</span>
                                    <span className={styles.ctxInherited}>
                                        (inherited)
                                    </span>
                                </div>
                            </div>
                        </div>

                        {/* Rejected City Layer */}
                        <div
                            ref={layerRejectedRef}
                            className={getLayerClasses("rejected")}
                            data-layer="rejected"
                            data-weight="1"
                        >
                            <div className={styles.layerHead}>
                                <span className={styles.layerDot}></span>
                                <span>
                                    City:{" "}
                                    <span className={styles.rejectedCity}>
                                        {ctxState.rejectedCity || ""}
                                    </span>
                                </span>
                                <span className={styles.layerWeight}>
                                    weight: 1
                                </span>
                                <span className={styles.rejectedBadge}>
                                    SKIP
                                </span>
                            </div>
                            <div className={styles.layerRows}>
                                <div
                                    className={`${styles.ctxRow} ${styles.rejectedRow}`}
                                    data-key="per_km_rate"
                                >
                                    <span>per_km_rate</span>
                                    <span className={styles.ctxVal}>
                                        {ctxState.rejectedCityVals
                                            ?.per_km_rate || ""}
                                    </span>
                                </div>
                                <div
                                    className={`${styles.ctxRow} ${styles.rejectedRow}`}
                                    data-key="surge_factor"
                                >
                                    <span>surge_factor</span>
                                    <span className={styles.ctxVal}>
                                        {ctxState.rejectedCityVals
                                            ?.surge_factor || ""}
                                    </span>
                                </div>
                            </div>
                        </div>

                        {/* Combo Layer */}
                        <div
                            ref={layerComboRef}
                            className={getLayerClasses("combo")}
                            data-layer="combo"
                            data-weight="3"
                        >
                            <div className={styles.layerHead}>
                                <span className={styles.layerDot}></span>
                                <span>
                                    City:{" "}
                                    <span className={styles.matchCity}>
                                        {ctxState.city}
                                    </span>
                                    , Vehicle:{" "}
                                    <span className={styles.matchVehicle}>
                                        {ctxState.vehicle}
                                    </span>
                                </span>
                                <span className={styles.layerWeight}>
                                    weight: 1+2=3
                                </span>
                            </div>
                            <div className={styles.layerRows}>
                                <div
                                    className={styles.ctxRow}
                                    data-key="per_km_rate"
                                >
                                    <span>per_km_rate</span>
                                    <span
                                        className={`${styles.ctxVal} ${styles.override}`}
                                    >
                                        {ctxState.comboVals.per_km_rate}
                                    </span>
                                    <span className={styles.ctxArrow}>→</span>
                                </div>
                                <div
                                    className={styles.ctxRow}
                                    data-key="surge_factor"
                                >
                                    <span>surge_factor</span>
                                    <span
                                        className={`${styles.ctxVal} ${styles.override}`}
                                    >
                                        {ctxState.comboVals.surge_factor}
                                    </span>
                                    <span className={styles.ctxArrow}>→</span>
                                </div>
                                <div
                                    className={styles.ctxRow}
                                    data-key="timeout_ms"
                                >
                                    <span>timeout_ms</span>
                                    <span
                                        className={`${styles.ctxVal} ${styles.override}`}
                                    >
                                        {ctxState.comboVals.timeout_ms}
                                    </span>
                                    <span className={styles.ctxArrow}>→</span>
                                </div>
                            </div>
                        </div>

                        {/* Rejected Combo Layer */}
                        <div
                            ref={layerRejected2Ref}
                            className={getLayerClasses("rejected2")}
                            data-layer="rejected2"
                            data-weight="3"
                        >
                            <div className={styles.layerHead}>
                                <span className={styles.layerDot}></span>
                                <span>
                                    City:{" "}
                                    <span className={styles.rejectedCity2}>
                                        {ctxState.rejectedCombo?.city || ""}
                                    </span>
                                    , Vehicle:{" "}
                                    <span className={styles.rejectedVehicle2}>
                                        {ctxState.rejectedCombo?.vehicle || ""}
                                    </span>
                                </span>
                                <span className={styles.layerWeight}>
                                    weight: 1+2=3
                                </span>
                                <span className={styles.rejectedBadge}>
                                    SKIP
                                </span>
                            </div>
                            <div className={styles.layerRows}>
                                <div
                                    className={`${styles.ctxRow} ${styles.rejectedRow}`}
                                    data-key="per_km_rate"
                                >
                                    <span>per_km_rate</span>
                                    <span className={styles.ctxVal}>
                                        {ctxState.rejectedComboVals
                                            ?.per_km_rate || ""}
                                    </span>
                                </div>
                                <div
                                    className={`${styles.ctxRow} ${styles.rejectedRow}`}
                                    data-key="surge_factor"
                                >
                                    <span>surge_factor</span>
                                    <span className={styles.ctxVal}>
                                        {ctxState.rejectedComboVals
                                            ?.surge_factor || ""}
                                    </span>
                                </div>
                            </div>
                        </div>
                    </div>

                    {/* Result */}
                    <div className={styles.result}>
                        <div className={styles.resultHead}>
                            Resolved Config
                            <span className={styles.resultWeight}>
                                (highest weight: {resValues.weight})
                            </span>
                        </div>
                        <div className={styles.resultVals}>
                            <div className={styles.resultItem}>
                                <span>per_km_rate</span>
                                <span>{resValues.per_km_rate}</span>
                            </div>
                            <div className={styles.resultItem}>
                                <span>surge_factor</span>
                                <span>{resValues.surge}</span>
                            </div>
                            <div className={styles.resultItem}>
                                <span>timeout_ms</span>
                                <span>{resValues.timeout}</span>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
}
