package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestCudppOperation extends FunSuite with TestUtils {

  test("sort(vector)") {
    val nativeA = NativeVector.vector(COLUMNS)
    val a = new Vector(nativeA.data)

    val sort = CudppSort.sort(a)
    val result = sort(a)
    sort.release()
    val nativeResult = NativeOperations.sort(nativeA)

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0)
  }

  test("sort desc") {
    val nativeA = NativeVector.vector(COLUMNS)
    val a = new Vector(nativeA.data)

    val sort = CudppSort.sort(a, SortOrder.DESC)
    val result = sort(a)
    sort.release()
    val nativeResult = NativeOperations.sort(nativeA)

    val maxError = compare(result.values(), nativeResult.data.reverse)
    assert(maxError === 0)
  }

  test("radixSort(vector)") {
    val nativeA = NativeVector.vector(COLUMNS)
    val a = new Vector(nativeA.data)

    val sort = CudppSort.radixSort(a)
    val result = sort(a)
    sort.release()
    val nativeResult = NativeOperations.sort(nativeA)

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0)
  }

  test("argsort(vector)") {
    val nativeA = NativeVector.vector(COLUMNS)
    val a = new Vector(nativeA.data)

    val argsort = CudppSort.argsort(a)
    val result = argsort(a)
    argsort.release()
    val nativeResult = NativeOperations.argsort(nativeA)

    val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
    assert(maxError === 0)
  }

  test("argsort(vector, desc)") {
    val nativeA = NativeVector.vector(COLUMNS)
    val a = new Vector(nativeA.data)

    val argsort = CudppSort.argsort(a, SortOrder.DESC)
    val result = argsort(a)
    argsort.release()
    val nativeResult = NativeOperations.argsort(nativeA)

    val maxError = compare(result.values().map(_.toFloat), nativeResult.data.reverse)
    assert(maxError === 0)
  }

  test("radixArgSort(vector)") {
    val nativeA = NativeVector.vector(COLUMNS)
    val a = new Vector(nativeA.data)

    val sort = CudppSort.radixArgsort(a)
    val result = sort(a)
    sort.release()
    val nativeResult = NativeOperations.argsort(nativeA)

    val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
    assert(maxError === 0)
  }

//  test("mergeSort") {
//    val nativeA = NativeVector.vector(COLUMNS)
//    val a = new Vector(nativeA.data)
//
//    val sort = CudppOperation.mergeSort(a)
//    val result = sort(a)
//    sort.release()
//    val nativeResult = NativeOperations.sort(nativeA)
//
//    val maxError = compare(result.values(), nativeResult.data)
//    assert(maxError === 0)
//  }

//  test("mergeArgSort") {
//    val nativeA = NativeVector.vector(COLUMNS)
//    val a = new Vector(nativeA.data)
//
//    val sort = CudppOperation.mergeArgsort(a)
//    val result = sort(a)
//    sort.release()
//    val nativeResult = NativeOperations.argsort(nativeA)
//
//    val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
//    assert(maxError === 0)
//  }

}
