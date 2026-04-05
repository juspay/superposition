import React, {
    createContext,
    useContext,
    useState,
    useCallback,
    ReactNode,
} from "react";

interface AnimationSlideContextValue {
    isPlaying: boolean;
    hasPlayed: boolean;
    replay: () => void;
    onComplete: () => void;
}

const AnimationSlideContext = createContext<AnimationSlideContextValue | null>(
    null,
);

interface AnimationSlideProviderProps {
    children: ReactNode;
    isPlaying: boolean;
    onReplay: () => void;
    onComplete: () => void;
}

export function AnimationSlideProvider({
    children,
    isPlaying,
    onReplay,
    onComplete,
}: AnimationSlideProviderProps) {
    const [hasPlayed, setHasPlayed] = useState(false);

    React.useEffect(() => {
        if (isPlaying && !hasPlayed) {
            setHasPlayed(true);
        }
    }, [isPlaying, hasPlayed]);

    const replay = useCallback(() => {
        setHasPlayed(false);
        onReplay();
    }, [onReplay]);

    return (
        <AnimationSlideContext.Provider
            value={{ isPlaying, hasPlayed, replay, onComplete }}
        >
            {children}
        </AnimationSlideContext.Provider>
    );
}

export function useAnimationSlide(): AnimationSlideContextValue {
    const context = useContext(AnimationSlideContext);
    if (!context) {
        throw new Error(
            "useAnimationSlide must be used within an AnimationSlide",
        );
    }
    return context;
}
