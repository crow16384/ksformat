#!/usr/bin/env python3
"""ksformat hero GIF — plot A "FORMAT MACHINE" (approved concept).
HISTORICAL: superseded by the author's own final hero animation
(vignettes/figures/ksformat-logo-hero.gif, 480x270, 76 frames, loop=1).
Rerunning this script overwrites those files with the older 550x298 render —
use it only to regenerate the original concept, not the shipped asset.

Raw codes (M/F/NA/17/42) stream into the hex engine (cream hexagon + navy
core, gold ring), each gets converted and a label (Male/Female/Unknown/
Child/Adult) pops out the other side; gold flash per conversion. After the
last conversion the whole scene dissolves into the settled lockup logo
(badge + ksformat wordmark + tagline) on white. Plays once (loop=1),
40 frames, 550x298 — durations/palette grammar mirrored from the ksTFL
hero GIF: frames 0-27 alternate 120/80 ms, 28-37 @40 ms, 38 @1600 ms hold,
39 @40 ms tail.
"""
from PIL import Image, ImageDraw, ImageFont, ImageFilter
import numpy as np, math

W, H, S = 550, 298, 3
NAVY   = (23, 49, 71)
NAVY2  = (38, 71, 96)
CREAM  = (238, 231, 216)
GOLD   = (213, 155, 61)
AMBER  = (241, 163, 64)
ORANGE = (217, 108, 44)
PAPER  = (248, 245, 239)
DARK2  = (28, 48, 70)
WHITE  = (255, 255, 255)
GLOW   = (126, 196, 233)

FA = "/System/Library/Fonts/Supplemental/Arial Bold.ttf"
FR = "/System/Library/Fonts/Supplemental/Arial.ttf"
_fc = {}
def fnt(path, sz):
    k = (path, round(sz))
    if k not in _fc: _fc[k] = ImageFont.truetype(path, max(1, round(sz)))
    return _fc[k]

def smooth(t): t = max(0.0, min(1.0, t)); return t * t * (3 - 2 * t)
def eout(t):   t = max(0.0, min(1.0, t)); return 1 - (1 - t) ** 3
def seg(f, a, b): return eout((f - a) / (b - a)) if f > a else 0.0
def sgn(f, a, b): return smooth((f - a) / (b - a)) if f > a else 0.0

def hex_path(cx, cy, r):
    return [(cx + r * math.cos(math.radians(60 * i + 90)),
             cy + r * math.sin(math.radians(60 * i + 90))) for i in range(6)]

# engine (centered)
EX, EY, ER = 252, 149, 54
CODES  = ["M", "F", "NA", "17", "42"]
LABELS = ["Male", "Female", "Unknown", "Child", "Adult"]
LANE_Y = [92, 120, 148, 176, 204]
CHIP_W = [26, 26, 34, 30, 30]
T0     = [3.5, 8.5, 13.5, 18.5, 23.5]
CH_X0, CH_L = -34, EX - ER + 10
LB_X0, LB_X1 = EX + ER - 4, 392

def bg_dark():
    yy, xx = np.mgrid[0:H, 0:W]
    r = np.sqrt(((xx - W*0.5)/(W*0.8))**2 + ((yy - H*0.1)/(H*0.95))**2)
    r = np.clip(r, 0, 1.4)[..., None] / 1.4
    b2 = np.array((8, 14, 23), np.float32)
    b1 = np.array(DARK2, np.float32)
    g = b2 + (b1 - b2) * (1 - r)
    im = Image.fromarray(g.astype(np.uint8)).resize((W*S, H*S), Image.LANCZOS)
    d = ImageDraw.Draw(im)
    rng = np.random.default_rng(7)
    for _ in range(70):
        x, y = int(rng.integers(0, W)), int(rng.integers(0, H))
        c = int(rng.integers(24, 44))
        d.ellipse([x*S, y*S, x*S+S, y*S+S], fill=(max(c-8, 0), c, c+12))
    return im

BG = bg_dark()

def engine(img, u, v):
    d = ImageDraw.Draw(img)
    pts = [(x*S, y*S) for x, y in hex_path(EX, EY, ER)]
    if u > 0 and v < 1.0:
        segs, tot = [], 0.0
        for i in range(6):
            p1, p2 = pts[i], pts[(i+1) % 6]
            L = math.dist(p1, p2); segs.append((p1, p2, L)); tot += L
        head, acc = u * tot, 0.0
        for p1, p2, L in segs:
            if acc >= head: break
            tl = min(1.0, (head - acc) / L)
            q = (p1[0]+(p2[0]-p1[0])*tl, p1[1]+(p2[1]-p1[1])*tl)
            d.line([p1, q], fill=GLOW, width=6*S)
            acc += L
    if v > 0:
        ov = Image.new("RGBA", img.size, (0,0,0,0)); od = ImageDraw.Draw(ov)
        od.polygon(pts, fill=CREAM+(int(255*v),))
        ip = [(x*S, y*S) for x, y in hex_path(EX, EY, ER-9)]
        od.polygon(ip, fill=NAVY2+(int(255*v),))
        gp = [(x*S, y*S) for x, y in hex_path(EX, EY, ER-5)]
        od.line(gp+[gp[0]], fill=GOLD+(int(215*v),), width=3*S)
        return Image.alpha_composite(img.convert("RGBA"), ov).convert("RGB")
    return img

def chip(img, x, y, w, text, fade=1.0, scale=1.0):
    if fade <= 0 or scale <= 0: return img
    wpx, hpx = w*scale, 20*scale
    ov = Image.new("RGBA", img.size, (0,0,0,0)); od = ImageDraw.Draw(ov)
    a = int(255*fade)
    od.rounded_rectangle([(x - wpx/2)*S, (y - hpx/2)*S,
                          (x + wpx/2)*S, (y + hpx/2)*S],
                         radius=int(5*scale*S), fill=PAPER+(a,))
    f = fnt(FA, 10.5*scale*S)
    bb = od.textbbox((0, 0), text, font=f)
    od.text((x*S - (bb[2]-bb[0])/2 - bb[0], y*S - (bb[3]-bb[1])/2 - bb[1]),
            text, font=f, fill=NAVY+(a,))
    return Image.alpha_composite(img.convert("RGBA"), ov).convert("RGB")

def label(img, x, y, text, a):
    if a <= 0: return img
    ov = Image.new("RGBA", img.size, (0,0,0,0)); od = ImageDraw.Draw(ov)
    od.text((x*S, (y-9.5)*S), text, font=fnt(FA, 14*S), fill=WHITE+(int(255*a),))
    od.line([((x-10)*S, y*S), ((x-4)*S, y*S)], fill=GOLD+(int(220*a),), width=2*S)
    return Image.alpha_composite(img.convert("RGBA"), ov).convert("RGB")

def flash(img, dt):
    k = max(0.0, 1 - abs(dt))
    if k <= 0: return img
    ov = Image.new("RGBA", img.size, (0,0,0,0)); od = ImageDraw.Draw(ov)
    r = ER + (1-k)*16
    od.ellipse([(EX-r)*S, (EY-r)*S, (EX+r)*S, (EY+r)*S],
               outline=(255, 224, 150, int(170*k)), width=int(3*S))
    ov = ov.filter(ImageFilter.GaussianBlur(max(1, int(4*S*k))))
    return Image.alpha_composite(img.convert("RGBA"), ov).convert("RGB")

def scene(f):
    img = BG.copy()
    u = seg(f, 0, 6)
    v = sgn(f, 4, 9)

    # chips are drawn BEFORE the engine so they slide UNDER the hexagon edge
    # (absorbed), never overlapping its outline
    if f < 33.5:
        for i in range(5):
            ta = T0[i] + 4.5
            w = CHIP_W[i]; ty = LANE_Y[i]
            if T0[i] <= f < ta:
                p = seg(f, T0[i], ta)
                img = chip(img, CH_X0 + (CH_L-CH_X0)*p, ty, w, CODES[i])
            elif ta <= f < ta + 1.2:
                p = (f - ta) / 1.2
                img = chip(img, CH_L - 4*p, ty, w, CODES[i], fade=1-p, scale=1-0.5*p)

    img = engine(img, u, v)

    if f < 33.5:
        for i in range(5):
            ta = T0[i] + 4.5
            ty = LANE_Y[i]
            if f >= ta + 0.35:
                p = seg(f, ta + 0.35, ta + 2.8)
                a = sgn(f, ta + 0.35, ta + 1.1)
                img = label(img, LB_X0 + (LB_X1-LB_X0)*p, ty, LABELS[i], a)
        for i in range(5):
            img = flash(img, f - (T0[i] + 4.5))

    k = sgn(f, 33.5, 35.5)
    if k > 0:
        img = Image.blend(img, SETTLED_S, k)
    return img

def settled(scale):
    """final lockup: badge left + wordmark right, on white.
    All font/line sizes are FINAL (550x298) px; scaled by `scale`."""
    px = lambda v: v * scale
    fs = lambda v: max(1, round(v * scale))
    img = Image.new("RGB", (W*scale, H*scale), WHITE)
    d = ImageDraw.Draw(img)
    bx, by, br = 96, 149, 60
    pts = [(px(x), px(y)) for x, y in hex_path(bx, by, br)]
    ip = [(px(x), px(y)) for x, y in hex_path(bx, by, br-12)]
    # layering mirrors logo.svg: hex fill -> inner gold ring -> navy outer edge
    # -> panel -> rows. Nothing is redrawn on top of the panel afterwards,
    # so the hexagon frame never cuts through the table.
    d.polygon(pts, fill=CREAM)
    d.line(ip+[ip[0]], fill=GOLD, width=fs(3))
    d.line(pts+[pts[0]], fill=NAVY, width=fs(7))
    l, t, r, b = px(bx-42), px(by-26), px(bx+42), px(by+26)
    d.rounded_rectangle([l, t, r, b], radius=fs(7), fill=NAVY2)
    # rows: code letter + amber connector + label bar (bar, not tiny text:
    # label text at badge scale is unreadable in a GIF)
    rows = [(bx-36, by-22, bx+36, by-11), (bx-36, by-5.5, bx+36, by+5.5), (bx-36, by+11, bx+36, by+22)]
    rowy = [by-16.5, by, by+16.5]
    codes = ["A", "I", "P"]
    for i in range(3):
        rl, rt, rr, rb = px(rows[i][0]), px(rows[i][1]), px(rows[i][2]), px(rows[i][3])
        d.rounded_rectangle([rl, rt, rr, rb], radius=fs(4), fill=PAPER)
        d.text((px(rows[i][0]+4), px(rowy[i]-7)), codes[i], font=fnt(FA, fs(11)), fill=NAVY)
        y = px(rowy[i])
        d.line([(px(rows[i][0]+15), y), (px(rows[i][0]+25), y)], fill=ORANGE, width=fs(4))
        d.rounded_rectangle([px(rows[i][0]+29), px(rowy[i]-4), px(rows[i][2]-4), px(rowy[i]+4)],
                            radius=fs(3), fill=(208,220,232))
    # wordmark block right (rule sized to the wordmark)
    wmk, tag = "ksformat", "PROC FORMAT for R"
    wm_font, tg_font = fnt(FA, fs(42)), fnt(FR, fs(18))
    wx, wy = 176, 100
    wb = d.textbbox((px(wx), px(wy)), wmk, font=wm_font)
    d.text((px(wx), px(wy)), wmk, font=wm_font, fill=NAVY)
    d.line([px(wx+2), px(158), wb[2], px(158)], fill=GOLD, width=fs(3.5))
    d.text((px(wx+2), px(172)), tag, font=tg_font, fill=(40,66,90))
    return img

SETTLED_S = settled(S)
SETTLED   = settled(1)

frames = []
for i in range(38):
    frames.append(scene(float(i)).resize((W, H), Image.LANCZOS).convert("RGB"))
frames.append(SETTLED)
frames.append(SETTLED)

DURS = [120 if i % 2 == 0 else 80 for i in range(28)] + [40]*10 + [1600, 40]

pal = frames[27].convert("P", palette=Image.ADAPTIVE, colors=256)
full_pal = pal.getpalette()
pframes = [fr_.quantize(palette=pal, dither=Image.Dither.FLOYDSTEINBERG) for fr_ in frames]
for pf in pframes:
    pf.putpalette(full_pal)

OUT = "man/figures/ksformat-logo-hero.gif"
pframes[0].save(OUT, save_all=True, append_images=pframes[1:],
                duration=DURS, loop=1, optimize=False)
print("saved", OUT)
for i in (3, 7, 12, 20, 27, 34, 38):
    frames[i].save(f"/tmp/chk_{i:02d}.png")
print("done")
