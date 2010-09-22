#!/usr/bin/env python

import sys, Image

NC = 10 # columns
NR = 8  # rows

LRAT = 0.15 # trim left ratio
RRAT = 0.03 # trim right ratio

def main():
  im = input()
  im = bw(im)
  im = crop(im)
  im = trim(im)
  rs = rows(im)
  cs = cells(rs)

  prog = decode(cs)
  s = [''.join(r) for r in prog]
  s = '\n'.join(s)
  print s

def input():
  im = Image.open(sys.argv[1])
  log('input', im)
  return im

def bw(im):
  if im.mode == 'RGB':
    src = im.load()
    w, h = im.size
    bw = Image.new('1', (w, h))
    snk = bw.load()

    for c in range(w):
      for r in range(h):
        if src[c, r][0] > 80 and \
           src[c, r][1] > 80 and \
           src[c, r][2] > 80:
          snk[c, r] = 1
        else:
          snk[c, r] = 0
  else:
    bw = im.convert('1')
  log('bw', bw)
  return bw

def crop(im):
  pxl = im.load()
  w, h = im.size
  cols = range(w)
  rows = range(h)
  n = w * h

  lft = 0
  for c in cols:
    a = avg([pxl[c, r] for r in rows])
    if a > 0.2:
      lft = c
      break

  top = 0
  for r in rows:
    a = avg([pxl[c, r] for c in cols])
    if a > 0.2:
      top = r
      break

  rht = w
  cols.reverse()
  for c in cols:
    a = avg([pxl[c, r] for r in rows])
    if a > 0.2:
      rht = c
      break

  bot = h
  rows.reverse()
  for r in rows:
    a = avg([pxl[c, r] for c in cols])
    if a > 0.2:
      bot = r
      break

  im = im.crop((lft, top, rht, bot))
  log('crop', im)
  return im

def trim(im):
  w, h = im.size
  l = int(w * LRAT)
  r = int(w - (w * RRAT))
  im = im.crop((l, 0, r, h))
  log('trim', im)
  return im

def rows(im):
  pxl = im.load()
  w, h = im.size

  def bound(i):
    return (i * h) / NR + (1 if i < (h % NR) else 0)

  rows = []
  for i in range(NR):
    t = bound(i)
    b = bound(i + 1)
    r = im.crop((0, t, w, b))
    log('row-%d' % i, r)
    rows.append(r)
  return rows

def cells(rows):
  cells = []
  for i in range(len(rows)):
    cs = cols(rows[i], i)
    cells.append(cs)
  return cells

def cols(row, n):
  pxl = row.load()
  w, h = row.size

  def bound(i):
    return (i * w) / NC + (1 if i < (w % NC) else 0)

  cols = []
  for i in range(NC):
    l = bound(i)
    r = bound(i + 1)
    c = row.crop((l, 0, r, h))
    log('cell-%d-%d' % (n, i), c)
    cols.append(c)
  return cols

def decode(card):
  prog = []
  for row in card:
    line = []
    for cell in row:
      c = entry(cell)
      line.append(c)
    prog.append(line)
  return prog

def entry(im):
  a = avg(im.getdata())
  if a > 0.95:
    return '-'
  else:
    return '1'

def sum(l):
  return reduce(lambda x, y: x + y, l)

def avg(l):
  t = sum(l) * 1.0
  n = len(l)
  return t / n

def log(s, im):
  im.save('log/%s.png' % s)

main()

