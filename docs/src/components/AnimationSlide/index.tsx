import React, {
    useState,
    useRef,
    useEffect,
    useCallback,
    ReactNode,
    CSSProperties,
} from "react";
import { AnimationSlideProvider } from "./AnimationSlideContext";
import styles from "./styles.module.css";

export interface AnimationSlideProps {
    width?: number | string;
    height?: number | string;
    mode?: "auto" | "click";
    loop?: boolean;
    loopDelay?: number;
    className?: string;
    children: ReactNode;
}

export default function AnimationSlide({
    width = "100%",
    height = 400,
    mode = "auto",
    loop = false,
    loopDelay = 2000,
    className,
    children,
}: AnimationSlideProps) {
    const containerRef = useRef<HTMLDivElement>(null);
    const [isPlaying, setIsPlaying] = useState(false);
    const [hasPlayed, setHasPlayed] = useState(false);
    const loopTimeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null);

    const play = useCallback(() => {
        setIsPlaying(true);
        setHasPlayed(true);
    }, []);

    const replay = useCallback(() => {
        // Clear any pending loop timeout
        if (loopTimeoutRef.current) {
            clearTimeout(loopTimeoutRef.current);
            loopTimeoutRef.current = null;
        }
        setIsPlaying(false);
        setTimeout(() => setIsPlaying(true), 10);
    }, []);

    const handleComplete = useCallback(() => {
        if (loop && hasPlayed) {
            loopTimeoutRef.current = setTimeout(() => {
                replay();
            }, loopDelay);
        }
    }, [loop, loopDelay, hasPlayed, replay]);

    // Cleanup loop timeout on unmount
    useEffect(() => {
        return () => {
            if (loopTimeoutRef.current) {
                clearTimeout(loopTimeoutRef.current);
            }
        };
    }, []);

    // Auto mode: IntersectionObserver
    useEffect(() => {
        if (mode !== "auto") return;
        const el = containerRef.current;
        if (!el) return;

        const observer = new IntersectionObserver(
            ([entry]) => {
                if (
                    entry.isIntersecting &&
                    entry.intersectionRatio >= 0.5 &&
                    !hasPlayed
                ) {
                    play();
                }
            },
            { threshold: 0.5 },
        );

        observer.observe(el);
        return () => observer.disconnect();
    }, [mode, hasPlayed, play]);

    const containerStyle: CSSProperties = {
        width: typeof width === "number" ? `${width}px` : width,
        height: typeof height === "number" ? `${height}px` : height,
    };

    return (
        <div
            ref={containerRef}
            className={`${styles.container} ${className || ""}`}
            style={containerStyle}
        >
            <AnimationSlideProvider
                isPlaying={isPlaying}
                onReplay={replay}
                onComplete={handleComplete}
            >
                {children}
            </AnimationSlideProvider>

            {mode === "click" && !isPlaying && (
                <button
                    className={styles.overlay}
                    onClick={play}
                    aria-label="Play animation"
                >
                    <span className={styles.playIcon}>▶</span>
                </button>
            )}

            {mode === "click" && hasPlayed && (
                <button
                    className={styles.replayBtn}
                    onClick={replay}
                    aria-label="Replay"
                >
                    ↻
                </button>
            )}
        </div>
    );
}
