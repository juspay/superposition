"""Soundtrack for the Superposition brag video. 120 BPM, A minor, bars start at 2.5s."""
import numpy as np, wave, sys

SR = 48000
DUR = 25.0
N = int(SR * DUR)
t = np.arange(N) / SR
rng = np.random.default_rng(3)
BEAT = 0.5
BAR0 = 2.5  # first downbeat


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
    'Dm': ([50, 57, 62, 65], 26),
    'E': ([52, 56, 59, 64], 28),
}
INC0, INC1 = 16.0, 19.5   # incident: dark section
bars = [(0.0, 'Fmaj7')]
s_, i = BAR0, 0
while s_ < INC0:
    bars.append((s_, ['Am', 'F', 'C', 'G'][i % 4])); s_ += 4 * BEAT; i += 1
bars += [(INC0, 'Dm'), (18.0, 'E'), (INC1, 'C'), (20.5, 'G'), (21.5, 'Fmaj7'), (23.0, 'C')]

def incident(x):
    return INC0 <= x < INC1

pad = np.zeros(N); bass = np.zeros(N); arp = np.zeros(N)
for bi, (st, ch) in enumerate(bars):
    en = bars[bi + 1][0] if bi + 1 < len(bars) else DUR
    notes, root = CHORDS[ch]
    dur = en - st + 0.6
    bright = 2.0 if st < BAR0 else 1.5 if incident(st) else 4.0
    att = 1.6 if st < BAR0 else 0.08
    for m in notes:
        for det in (-0.004, 0.004):
            sig = additive(mtof(m), dur, bright, detune=det)
            sig *= env_adsr(len(sig), att, 0.3, 0.8, 0.5, en - st)[: len(sig)]
            place(pad, sig, st, 0.05 if not incident(st) else 0.06)
    if BAR0 <= st < 21.5:
        k = 0
        step = BEAT / 2
        while st + k * step < min(en, 21.5):
            at = st + k * step
            f = mtof(root + (12 if k % 4 == 3 and not incident(at) else 0))
            n = int(0.22 * SR); tt = np.arange(n) / SR
            b = (np.sin(2 * np.pi * f * tt) + 0.25 * np.sin(4 * np.pi * f * tt)) * np.exp(-6 * tt) * np.minimum(1, tt / 0.004) * np.clip((0.22 - tt) / 0.03, 0, 1)
            place(bass, b, at, (0.32 if k % 2 == 0 else 0.22) * (1.1 if incident(at) else 1))
            k += 1
    if not incident(st) and st < 23.0:
        tones = [m + 12 for m in notes]
        pattern = [0, 2, 1, 3, 2, 1, 3, 2]
        k = 0
        while st + k * BEAT / 4 < min(en, 23.0):
            at = st + k * BEAT / 4
            if incident(at):
                break
            m = tones[pattern[k % 8] % len(tones)]
            g = 0.05 if at < BAR0 or at >= 21.5 else 0.075
            if k % 4 == 0:
                g *= 1.35
            place(arp, pluck(mtof(m), 0.35, 14, 0.6), at, g)
            k += 1
arp *= np.clip(t / 1.2, 0, 1)

# ---------------- drums ----------------
drums = np.zeros(N)
duck = np.ones(N)
def kick_at(b, g=0.75):
    place(drums, kick(), b, g)
    i = int(b * SR); seg = duck[i:i + int(0.25 * SR)]
    seg *= 1 - 0.45 * np.exp(-np.arange(len(seg)) / SR * 14)
HAT = lambda: fft_filter(noise_hit(0.06, 70), lo=6000, hi=11000) * np.minimum(1, np.arange(int(0.06*SR)) / (0.002*SR))
CLAP = lambda: fft_filter(noise_hit(0.18, 22, hp=False), lo=900, hi=5000) * np.minimum(1, np.arange(int(0.18*SR)) / (0.002*SR))
b = BAR0
while b < 21.5:
    if incident(b):
        if (b - INC0) % 1.0 < 1e-6:
            kick_at(b, 0.6)   # half-time heartbeat
    else:
        kick_at(b)
        place(drums, HAT(), b + BEAT / 2, 0.045)
        if b >= 6.5 and ((b - BAR0) / BEAT) % 4 in (1, 3):
            place(drums, CLAP(), b, 0.10)
    b += BEAT

# ---------------- SFX (in key: A minor) ----------------
sfx = np.zeros(N)

def whoosh(end, length=0.5, gain=0.08):
    n = int(length * SR); tt = np.arange(n) / SR
    x = fft_filter(rng.standard_normal(n), lo=600, hi=5000)
    e = (tt / length) ** 2.5 * (1 - np.clip((tt - length + 0.03) / 0.03, 0, 1))
    place(sfx, x * e, end - length, gain)

def ticks(a, b, step, gain):
    x = a
    while x < b:
        place(sfx, fft_filter(noise_hit(0.025, 260), lo=2000, hi=6500), x + rng.uniform(-0.006, 0.006), gain * rng.uniform(0.7, 1.0))
        x += step

def thud(f=mtof(38), dur=0.5):
    n = int(dur * SR); tt = np.arange(n) / SR
    return np.sin(2 * np.pi * f * tt) * np.exp(-10 * tt) * np.minimum(1, tt / 0.002)

for cut, ln in ((2.5, 0.5), (11.5, 0.6), (16.0, 0.5), (21.5, 0.5)):
    whoosh(cut, ln, 0.06)
# hook: logo lands, quantum chips blink in
place(sfx, thud(mtof(45), 0.4), 0.18, 0.25)
for j, m in enumerate((76, 79, 81, 84)):
    place(sfx, pluck(mtof(m + 12), 0.3, 16, 0.5), 0.35 + j * 0.08, 0.05)
# one line typed
ticks(3.25, 4.7, 0.048, 0.045)
place(sfx, pluck(mtof(76), 0.8, 7, 0.8), 4.75, 0.12)
place(sfx, pluck(mtof(81), 0.8, 7, 0.8), 4.83, 0.10)
ticks(5.0, 5.35, 0.045, 0.035)
place(sfx, bell(mtof(88), 2.0), 5.55, 0.06)
for j, a in enumerate((5.0, 5.2, 5.4)):
    place(sfx, thud(mtof([45, 48, 52][j]), 0.3), a, 0.22)
# toml edit
place(sfx, fft_filter(noise_hit(0.03, 200), lo=1500, hi=5000), 7.5, 0.06)
place(sfx, fft_filter(noise_hit(0.03, 200), lo=1500, hi=5000), 8.95, 0.06)
ticks(9.3, 9.32, 0.05, 0.06)
place(sfx, bell(mtof(81), 2.5), 9.6, 0.09)
place(sfx, bell(mtof(88), 2.5), 9.63, 0.05)
# platform
ticks(12.0, 12.4, 0.09, 0.04)
for j in range(5):
    place(sfx, pluck(mtof([69, 72, 76, 79, 81][j]), 0.5, 10, 0.6), 12.57 + j * 0.12, 0.08)
for j in range(6):
    place(sfx, pluck(mtof([81, 84, 88][j % 3] + 12), 0.25, 20, 0.3), 13.5 + j * 0.06, 0.02)
    place(sfx, pluck(mtof([84, 88, 93][j % 3] + 12), 0.25, 20, 0.3), 13.9 + j * 0.06, 0.02)
place(sfx, thud(mtof(33), 0.5), 13.75, 0.3)
# incident
for j, m in enumerate((76, 77)):
    place(sfx, pluck(mtof(m), 0.6, 6, 0.4), 18.0 + j * 0.16, 0.08)
ticks(18.24, 18.26, 0.05, 0.05); ticks(18.74, 18.76, 0.05, 0.05)
ticks(18.89, 19.44, 0.035, 0.04)
place(sfx, bell(mtof(84), 2.5), 19.84, 0.08)
place(sfx, bell(mtof(91), 2.5), 19.87, 0.05)
place(sfx, thud(mtof(36), 0.4), 20.15, 0.25)
# outro
place(sfx, bell(mtof(77), 3.5), 21.62, 0.08)
place(sfx, bell(mtof(84), 3.5), 21.65, 0.06)
place(sfx, bell(mtof(88), 3.5), 21.68, 0.04)

# ---------------- mix ----------------
music_dry = pad * duck + bass * 1.0 + arp
wet = reverb(pad * 0.6 + arp * 0.8 + sfx * 0.9 + drums * 0.15, 2.4)
dry = music_dry + drums + sfx * 0.9
wetR = np.roll(wet, int(0.013 * SR))
st = np.stack([dry + wet * 0.35, dry + wetR * 0.35], axis=1)
for c in range(2):
    st[:, c] = fft_filter(st[:, c], lo=30)
env = np.clip(t / 0.3, 0, 1) * np.clip((DUR - t) / 1.4, 0, 1) ** 1.5
st *= env[:, None]
st /= np.max(np.abs(st)) + 1e-9
st = np.tanh(st * 1.6) / np.tanh(1.6)
st *= 10 ** (-1.0 / 20)
pcm = (np.clip(st, -1, 1) * 32767).astype(np.int16)
with wave.open(sys.argv[1], 'wb') as w:
    w.setnchannels(2); w.setsampwidth(2); w.setframerate(SR)
    w.writeframes(pcm.tobytes())
print('peak', np.max(np.abs(st)), 'rms dB', 20 * np.log10(np.sqrt(np.mean(st ** 2))))
