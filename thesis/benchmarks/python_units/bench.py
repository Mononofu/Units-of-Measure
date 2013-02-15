#!/usr/bin/python
import time
from units import unit

BATCH_SIZE = 500000
BATCH_WARMUP = 3
BATCH_NUM = 20

def timer(f):
  start = time.time()
  f()
  return time.time() - start


class Benchmark:
  def executeBatch(self):
    def addition():
      for i in range(BATCH_SIZE):
        self.c[i] = self.a[i] + self.b[i]

    def multiplication():
      for i in range(BATCH_SIZE):
        self.c[i] = self.a[i] * self.b[i]

    return (timer(addition), timer(multiplication))



class BenchmarkInt(Benchmark):
  def __init__(self):
    self.a = [int(i) for i in range(BATCH_SIZE)]
    self.b = [int(i) for i in range(BATCH_SIZE)]
    self.c = [int(i) for i in range(BATCH_SIZE)]


class BenchmarkFloat(Benchmark):
  def __init__(self):
    self.a = [float(i) for i in range(BATCH_SIZE)]
    self.b = [float(i) for i in range(BATCH_SIZE)]
    self.c = [float(i) for i in range(BATCH_SIZE)]


class BenchmarkUnits(Benchmark):
  def __init__(self):
    m = unit('m')
    self.a = [m(i) for i in range(BATCH_SIZE)]
    self.b = [m(i) for i in range(BATCH_SIZE)]
    self.c = [m(i) for i in range(BATCH_SIZE)]


benchmarks = [BenchmarkInt(), BenchmarkFloat(), BenchmarkUnits()]
print "Starting benchmarks ..."
for b in benchmarks:
  for i in range(BATCH_WARMUP):
    b.executeBatch()
  results = []
  for i in range(BATCH_NUM):
    results.append(b.executeBatch())

  times = reduce(lambda a, b: map(lambda x: x[0] + x[1], zip(a, b)), results)
  print "%s: %.2f ns add, %.2f ns mult" % (b.__class__.__name__,
    times[0] * 1e9 / BATCH_NUM / BATCH_SIZE,
    times[1] * 1e9 / BATCH_NUM / BATCH_SIZE)


