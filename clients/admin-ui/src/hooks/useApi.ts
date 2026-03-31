import { useCallback, useEffect, useRef, useState } from "react";

export interface UseApiState<T> {
  data: T | null;
  loading: boolean;
  error: string | null;
}

export interface UseApiResult<T> extends UseApiState<T> {
  refetch: () => void;
}

/**
 * Hook for fetching data from the API with automatic loading/error state.
 */
export function useApi<T>(
  fetcher: () => Promise<T>,
  deps: unknown[] = [],
): UseApiResult<T> {
  const [state, setState] = useState<UseApiState<T>>({
    data: null,
    loading: true,
    error: null,
  });

  const fetcherRef = useRef(fetcher);
  fetcherRef.current = fetcher;

  const load = useCallback(() => {
    setState((prev) => ({ ...prev, loading: true, error: null }));
    fetcherRef
      .current()
      .then((data) => setState({ data, loading: false, error: null }))
      .catch((err) =>
        setState({
          data: null,
          loading: false,
          error: err instanceof Error ? err.message : String(err),
        }),
      );
  }, []);

  useEffect(() => {
    load();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, deps);

  return { ...state, refetch: load };
}

/**
 * Hook for mutations (create/update/delete) with loading/error state.
 */
export function useMutation<TArgs extends unknown[], TResult>(
  mutationFn: (...args: TArgs) => Promise<TResult>,
): {
  mutate: (...args: TArgs) => Promise<TResult>;
  loading: boolean;
  error: string | null;
} {
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const mutate = useCallback(
    async (...args: TArgs): Promise<TResult> => {
      setLoading(true);
      setError(null);
      try {
        const result = await mutationFn(...args);
        return result;
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        setError(msg);
        throw err;
      } finally {
        setLoading(false);
      }
    },
    [mutationFn],
  );

  return { mutate, loading, error };
}
