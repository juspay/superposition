"""Soundtrack for the Superposition brag video. 120 BPM, A minor, bars start at 3.0s."""
import numpy as np, wave, sys

SR = 48000
DUR = 22.0
N = int(SR * DUR)
t = np.arange(N) / SR
rng = np.random.default_rng(3)
BEAT = 0.5
BAR0 = 3.0  # first downbeat


def mtof(m):
    return 440.0 * 2 ** ((m - 69) / 12)


def env_adsr(n, a, d, s, r, hold):
    a, d, r = int(a * SR), int(d * SR), int(r * SR)
    h = max(int(hold * SR), a + d)
    e = np.zeros(h + r)
    e[:a] = np.linspace(0, 1, a, endpoint=False) if a else 1
    e[a:a + d] = np.linspace(1, s, d, endpoint=False)
    e[a + d:h] = s
    e[h:] = np.linspace(s, 0, r) if r else 0
    if n and len(e) < n:
        e = np.concatenate([e, np.zeros(n - len(e))])
    return e[:n] if n else e


def place(buf, sig, at, gain=1.0):
    i = int(at * SR)
    if i >= len(buf):
        return
    j = min(len(buf), i + len(sig))
    buf[i:j] += sig[: j - i] * gain


def additive(freq, dur, bright, nharm=14, detune=0.0):
    """Soft saw: harmonics 1/n with a gentle lowpass at freq*bright."""
    n = int(dur * SR)
    tt = np.arange(n) / SR
    out = np.zeros(n)
    for k in range(1, nharm + 1):
        f = freq * k
        if f > 12000:
            break
        lp = 1.0 / (1.0 + (f / (freq * bright)) ** 4)
        ph = rng.uniform(0, 2 * np.pi)
        out += np.sin(2 * np.pi * f * (1 + detune) * tt + ph) * lp / k
    return out


def pluck(freq, dur=0.6, decay=7.0, bright=1.0):
    n = int(dur * SR)
    tt = np.arange(n) / SR
    e = np.exp(-decay * tt) * np.minimum(1, tt / 0.003)
    s = np.sin(2 * np.pi * freq * tt) + 0.35 * bright * np.sin(2 * np.pi * 2 * freq * tt) * np.exp(-12 * tt) \
        + 0.12 * bright * np.sin(2 * np.pi * 3 * freq * tt) * np.exp(-20 * tt)
    return s * e


def bell(freq, dur=2.5):
    n = int(dur * SR)
    tt = np.arange(n) / SR
    parts = [(1, 1.0, 2.2), (2.0, 0.45, 3.5), (2.76, 0.25, 5.0), (5.4, 0.08, 8.0)]
    s = sum(a * np.sin(2 * np.pi * freq * r * tt) * np.exp(-d * tt) for r, a, d in parts)
    return s * np.minimum(1, tt / 0.002)


def kick(dur=0.35):
    n = int(dur * SR)
    tt = np.arange(n) / SR
    f = 48 + 90 * np.exp(-tt * 30)
    ph = 2 * np.pi * np.cumsum(f) / SR
    return np.sin(ph) * np.exp(-tt * 9) * np.minimum(1, tt / 0.004)


def noise_hit(dur, decay, hp=True):
    n = int(dur * SR)
    tt = np.arange(n) / SR
    x = rng.standard_normal(n)
    if hp:
        x = np.diff(np.concatenate([[0], x]))  # crude highpass
    return x * np.exp(-decay * tt)


def fft_filter(x, lo=None, hi=None):
    X = np.fft.rfft(x)
    f = np.fft.rfftfreq(len(x), 1 / SR)
    m = np.ones_like(f)
    if lo:
        m *= 1 / (1 + (lo / np.maximum(f, 1)) ** 4)
    if hi:
        m *= 1 / (1 + (f / hi) ** 4)
    return np.fft.irfft(X * m, len(x))


def reverb(x, secs=2.2, damp=6000):
    n = int(secs * SR)
    tt = np.arange(n) / SR
    ir = rng.standard_normal(n) * np.exp(-3.2 * tt / secs * 2)
    ir = fft_filter(ir, lo=200, hi=damp)
    ir[: int(0.012 * SR)] = 0  # predelay
    ir /= np.sqrt(np.sum(ir ** 2))
    L = len(x) + n
    y = np.fft.irfft(np.fft.rfft(x, L) * np.fft.rfft(ir, L), L)[: len(x)]
    return y


# ---------------- harmony ----------------
CHORDS = {  # pad voicing, bass root
    'Am': ([57, 60, 64, 69], 33),
    'F': ([53, 57, 60, 64], 29),
    'C': ([55, 60, 64, 67], 36),
    'G': ([55, 59, 62, 67], 31),
    'Fmaj7': ([53, 57, 60, 64], 29),
}
prog = ['Am', 'F', 'C', 'G']
bars = []  # (start, chord)
bars.append((0.0, 'Fmaj7'))
s = BAR0
i = 0
while s < DUR:
    bars.append((s, prog[i % 4]))
    s += 4 * BEAT
    i += 1
# final chord rings from bar at 19.0 (Am) — hold 'F' at 21? keep Am resolve at 19 and let it ring
bars = [(st, ch) for st, ch in bars if st < 19.0] + [(19.0, 'Am')]

pad = np.zeros(N)
bass = np.zeros(N)
arp = np.zeros(N)
for bi, (st, ch) in enumerate(bars):
    en = bars[bi + 1][0] if bi + 1 < len(bars) else DUR
    notes, root = CHORDS[ch]
    dur = en - st + 0.6
    # brightness: closed in the intro, opening into the drop
    bright = 2.2 if st < BAR0 else 4.0
    for m in notes:
        for det in (-0.004, 0.004):
            sig = additive(mtof(m), dur, bright, detune=det)
            sig *= env_adsr(len(sig), 0.08 if st >= BAR0 else 1.6, 0.3, 0.8, 0.5, en - st)[: len(sig)]
            place(pad, sig, st, 0.05)
    # bass: 8ths from the drop, soft sine + 2nd harmonic
    if st >= BAR0:
        k = 0
        while st + k * BEAT / 2 < min(en, 20.0):
            at = st + k * BEAT / 2
            f = mtof(root + (12 if k % 4 == 3 else 0))
            n = int(0.22 * SR)
            tt = np.arange(n) / SR
            b = (np.sin(2 * np.pi * f * tt) + 0.25 * np.sin(4 * np.pi * f * tt)) * np.exp(-6 * tt) * np.minimum(1, tt / 0.004) * np.clip((0.22 - tt) / 0.03, 0, 1)
            place(bass, b, at, 0.32 if k % 2 == 0 else 0.22)
            k += 1
    # arpeggio: chord tones up an octave, 16ths
    tones = [m + 12 for m in notes]
    pattern = [0, 2, 1, 3, 2, 1, 3, 2]
    k = 0
    while st + k * BEAT / 4 < min(en, 21.0):
        at = st + k * BEAT / 4
        m = tones[pattern[k % 8] % len(tones)]
        g = 0.05 if at < BAR0 else 0.075
        if k % 4 == 0:
            g *= 1.35
        place(arp, pluck(mtof(m), 0.35, 14, 0.6), at, g)
        k += 1

# intro arp fades in from 0
arp *= np.clip(t / 1.2, 0, 1)

# ---------------- drums ----------------
drums = np.zeros(N)
duck = np.ones(N)
b = BAR0
while b < 20.0:
    place(drums, kick(), b, 0.75)
    i = int(b * SR)
    dl = int(0.25 * SR)
    seg = duck[i:i + dl]
    seg *= 1 - 0.45 * np.exp(-np.arange(len(seg)) / SR * 14)
    # offbeat hat
    place(drums, fft_filter(noise_hit(0.06, 70), lo=6000, hi=11000) * np.minimum(1, np.arange(int(0.06*SR)) / (0.002*SR)), b + BEAT / 2, 0.045)
    b += BEAT
# clap on 2 & 4 from scene 3 onwards
b = 7.0
while b < 20.0:
    for off in (BEAT, 3 * BEAT):
        c = fft_filter(noise_hit(0.18, 22, hp=False), lo=900, hi=5000) * np.minimum(1, np.arange(int(0.18*SR)) / (0.002*SR))
        place(drums, c, b + off, 0.10)
    b += 4 * BEAT

# ---------------- SFX (in key: A minor) ----------------
sfx = np.zeros(N)


def whoosh(end, length=0.5, gain=0.08):
    n = int(length * SR)
    tt = np.arange(n) / SR
    x = fft_filter(rng.standard_normal(n), lo=600, hi=5000)
    e = (tt / length) ** 2.5 * (1 - np.clip((tt - length + 0.03) / 0.03, 0, 1))
    place(sfx, x * e, end - length, gain)


for cut in (3.0, 6.0, 11.0, 15.0, 18.0):
    whoosh(cut, 0.55, 0.05)

# logo pop at 3.0: bell on A + E
place(sfx, bell(mtof(81)), 3.02, 0.10)
place(sfx, bell(mtof(88)), 3.02, 0.05)

# typing ticks: context (6.55–7.65), delete (11.95–12.25), retype (12.25–12.9)
def ticks(a, b, step, gain):
    x = a
    while x < b:
        place(sfx, fft_filter(noise_hit(0.025, 260), lo=2000, hi=6500), x + rng.uniform(-0.008, 0.008), gain * rng.uniform(0.7, 1.0))
        x += step
ticks(6.55, 7.65, 0.065, 0.05)
ticks(11.95, 12.25, 0.045, 0.035)
ticks(12.25, 12.9, 0.06, 0.05)

# layers stacking: ascending plucks E5 A5 (rejected: muted C5) E6? -> E5, A5, C5(muted), C6
for at, m, g in ((7.65, 76, 0.13), (8.05, 81, 0.13), (8.45, 72, 0.06), (8.85, 84, 0.14)):
    place(sfx, pluck(mtof(m), 0.8, 8, 0.8), at, g)
# resolve chime
place(sfx, bell(mtof(81), 3.0), 9.35, 0.11)
place(sfx, bell(mtof(88), 3.0), 9.38, 0.07)
place(sfx, bell(mtof(93), 3.0), 9.41, 0.04)

# reject: soft low thud on D + dull knock
def thud(f=mtof(38), dur=0.5):
    n = int(dur * SR); tt = np.arange(n) / SR
    return np.sin(2 * np.pi * f * tt) * np.exp(-10 * tt) * np.minimum(1, tt / 0.002)
place(sfx, thud(), 12.95, 0.35)
for i, at in enumerate((13.0, 13.18, 13.36)):
    place(sfx, pluck(mtof([69, 63, 63][i]), 0.4, 14, 0.4), at, 0.08 if i else 0.10)
place(sfx, thud(mtof(33), 0.6), 13.55, 0.45)
place(sfx, fft_filter(noise_hit(0.12, 40, hp=False), hi=1800), 13.55, 0.12)

# experiment buckets: soft plucks rising
for j in range(10):
    place(sfx, pluck(mtof([69, 72, 76, 79, 81][j % 5] + 12), 0.3, 18, 0.3), 15.95 + j * 0.03 * 1.0 + j * 0.0, 0.025)
    place(sfx, pluck(mtof([76, 79, 81, 84, 88][j % 5] + 12), 0.3, 18, 0.3), 16.45 + j * 0.03, 0.025)

# outro: chord bloom + bell
place(sfx, bell(mtof(69), 4.0), 18.02, 0.09)
place(sfx, bell(mtof(76), 4.0), 18.05, 0.06)
place(sfx, bell(mtof(81), 4.0), 18.08, 0.05)

# ---------------- mix ----------------
music_dry = pad * duck + bass * 1.0 + arp
wet = reverb(pad * 0.6 + arp * 0.8 + sfx * 0.9 + drums * 0.15, 2.4)
dry = music_dry + drums + sfx * 0.9
wetR = np.roll(wet, int(0.013 * SR))
st = np.stack([dry + wet * 0.35, dry + wetR * 0.35], axis=1)
for c in range(2):
    st[:, c] = fft_filter(st[:, c], lo=30)
env = np.clip(t / 0.3, 0, 1) * np.clip((DUR - t) / 1.6, 0, 1) ** 1.5
st *= env[:, None]
st /= np.max(np.abs(st)) + 1e-9
st = np.tanh(st * 1.6) / np.tanh(1.6)
st *= 10 ** (-1.0 / 20)
pcm = (np.clip(st, -1, 1) * 32767).astype(np.int16)
with wave.open(sys.argv[1], 'wb') as w:
    w.setnchannels(2); w.setsampwidth(2); w.setframerate(SR)
    w.writeframes(pcm.tobytes())
print('peak', np.max(np.abs(st)), 'rms dB', 20 * np.log10(np.sqrt(np.mean(st ** 2))))
