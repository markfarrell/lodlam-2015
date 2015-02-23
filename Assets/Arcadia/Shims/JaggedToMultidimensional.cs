using UnityEngine;
using System;
using System.Collections;

public class JaggedToMultidimensional {

  /**
   * No Clojure CLR interop support for multidimensional arrays.
   * Needed to create a shim to convert jagged 2d arrays to
   * multidimensional arrays in order to call `TerrainData.SetHeights`.
   **/
  public static float[,] ConvertFloats(float[][] arr) {

    if (arr == null) {
      throw new ArgumentNullException("arr == null!");
    }

    float[,] arr2;

    int width = arr.Length;
    int height = 0;

    for(var i = 0; i < arr.Length; i++) {

      if(arr[i].Length > height) {

        height = arr[i].Length;

      }

    }

    arr2 = new float[width, height];

    for(var i = 0; i < arr.Length; i++) {

      for(var j = 0; j < arr[i].Length; j++) {

        arr2[i, j] = arr[i][j];

      }

    }

    return arr2;

  }

}
