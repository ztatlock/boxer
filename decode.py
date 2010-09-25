#!/usr/bin/env python

import sys, time, Image

NC = 10 # columns
NR = 8  # rows

LRAT = 0.15 # trim left ratio
RRAT = 0.03 # trim right ratio

BKGRND  = 25  # max avg pxl of background row/col
PUNCHED = 240 # min avg pxl of punched cell

def main():
  log('init')
  im = input()
  im = bw(im)
  im = crop(im)
  im = trim(im)
  rs = rows(im)
  cs = cells(rs)
  prg = decode(cs)
  print_prog(prg)
  log('done')
  write_log()

def input():
  im = Image.open(sys.argv[1])
  log('input read')
  ilog('input', im)
  return im

def bw(im):
  if im.mode == 'RGB':
    im = im.point(lambda x: 0 if x < 130 else 255)
  bw = im.convert('L')
  log('converted to bw')
  ilog('bw', bw)
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
    if a > BKGRND:
      lft = c
      break

  top = 0
  for r in rows:
    a = avg([pxl[c, r] for c in cols])
    if a > BKGRND:
      top = r
      break

  rht = w
  cols.reverse()
  for c in cols:
    a = avg([pxl[c, r] for r in rows])
    if a > BKGRND:
      rht = c
      break

  bot = h
  rows.reverse()
  for r in rows:
    a = avg([pxl[c, r] for c in cols])
    if a > BKGRND:
      bot = r
      break

  im = im.crop((lft, top, rht, bot))
  log('cropped')
  ilog('crop', im)
  return im

def trim(im):
  w, h = im.size
  l = int(w * LRAT)
  r = int(w - (w * RRAT))
  im = im.crop((l, 0, r, h))
  log('trimmed')
  ilog('trim', im)
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
    ilog('row-%d' % i, r)
    rows.append(r)
  log('extracted rows')
  return rows

def cells(rows):
  cells = []
  for i in range(len(rows)):
    cs = cols(rows[i], i)
    cells.append(cs)
  log('extracted cells')
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
    ilog('cell-%d-%d' % (n, i), c)
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
  log('decoded')
  return prog

def entry(im):
  a = avg(im.getdata())
  if a < PUNCHED:
    return '1'
  else:
    return '-'

def print_prog(prg):
  s = [''.join(r) for r in prg]
  s = '\n'.join(s)
  print s

def avg(l):
  t = 0.0
  for e in l:
    t += e
  return t / len(l)

lbuf = []
lbuf.append(time.strftime('%y-%m-%d %H:%M:%S'))
def log(msg):
  t = '%0.2f ' % time.clock()
  lbuf.append(t + msg)

def write_log():
  f = open('log/decode', 'w')
  f.write('\n'.join(lbuf))
  f.close()

def ilog(s, im):
  im.save('log/%s.png' % s)

main()

#import cProfile
#cProfile.run('main()')

