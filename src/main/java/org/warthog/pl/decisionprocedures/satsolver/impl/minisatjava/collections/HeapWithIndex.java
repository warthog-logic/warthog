package org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections;

import java.lang.reflect.Array;

public class HeapWithIndex<T extends ComparableWithIndex<T>> extends Heap<T> {
  public HeapWithIndex() {
    super();
  }

  protected int find(T a, int index) {
    if (a != null) { return a.index(); }
    return -1;
  }

  public void clear() {
    for (int i = 0; i < heapsize; i++)
      if (heap[i] != null) { heap[i].setIndex(-1); }
    super.clear();
  }

  @SuppressWarnings("unchecked")
  public void insert(T a) {
    if (maybe_inconsistent) { quickInsert(a); } else {
      heapsize++;
      if (heap == null || heap.length < heapsize) {
        T[] _heap = (T[]) Array.newInstance(a.getClass(), (heap == null ? 2 : heap.length * 2));
        if (heap != null) { System.arraycopy(heap, 0, _heap, 0, heap.length); }
        heap = _heap;
      }
      heap[heapsize - 1] = a;
      a.setIndex(heapsize - 1);
      heapIncreaseKey(heapsize - 1);
    }
  }

  protected void heapIncreaseKey(int index) {
    int i = index;
    while (i > 0 && parent(i).compareTo(heap[i]) < 0) {
      swap(parentInd(i), i);
      i = parentInd(i);
    }
  }

  protected void swap(int pos0, int pos1) {
    if (pos0 != pos1 && pos0 < heapsize && pos1 < heapsize) {
      heap[pos0].setIndex(pos1);
      heap[pos1].setIndex(pos0);
    }
    super.swap(pos0, pos1);
  }

  public void quickInsert(T a) {
    super.quickInsert(a);
    heap[heapsize - 1].setIndex(heapsize - 1);
  }

  protected void quickDelete(int index) {
    if (heapsize < 1 || index < 0 || heapsize <= index) { return; }
    if (!maybe_inconsistent) { maybe_inconsistent = true; }
    heap[index].setIndex(-1);
    if (index == heapsize - 1) { heapsize--; } else {
      heap[index] = heap[heapsize - 1];
      heap[index].setIndex(index);
      heapsize--;
    }
  }

  protected void delete(int index) {
    if (maybe_inconsistent) { quickDelete(index); } else {
      if (heapsize < 1 || index < 0 || heapsize <= index) { return; }
      heap[index].setIndex(-1);
      if (index == heapsize - 1) { heapsize--; } else {
        T old = heap[index];
        heap[index] = heap[heapsize - 1];
        heap[index].setIndex(index);
        if (heapsize > 1) {
          heapsize--;
          if (heap[index].compareTo(old) > 0) { heapIncreaseKey(index); } else { heapDecreaseKey(index); }
        } else { heapsize = 0; }
      }
    }
  }

  public T heapExtractMax() {
    if (heapsize < 1) { return null; }
    if (maybe_inconsistent) { /* restore heap property destroyed by quick_{delete,insert} */
      restoreHeapProperty();
      maybe_inconsistent = false;
    }
    T max = heap[0];
    max.setIndex(-1);
    if (heapsize > 1) { /* no need*/
      heap[0] = heap[heapsize - 1];
      heap[0].setIndex(0);
      heapsize--;
      heapDecreaseKey(0);
    } else /* heapsize==1 */ { heapsize = 0; }
    return max;
  }
}
