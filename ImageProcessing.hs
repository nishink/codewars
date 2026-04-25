-- https://www.codewars.com/kata/5239078120eeabe18f0000da
-- Image Processing

module Haskell.Codewars.ImageProcessing where

-- 型定義
data Matrix a = Matrix { width :: Int, height :: Int, dat :: [[a]] } deriving (Show)
type Image  = Matrix Int
type Kernel = Matrix Double

-- 画像処理関数
processImage :: Image -> Kernel -> Image
processImage img kernel = Matrix w h processedData
  where
    w = width img
    h = height img
    processedData = [[applyKernel x y | x <- [0..w-1]] | y <- [0..h-1]]

-- カーネル適用関数
-- 画像データの指定座標にカーネルを適用して、新しい値を返す
-- 画像データの範囲外の座標に対しては、最も近い画素の値を使う
applyKernel :: Image -> Kernel -> Int -> Int -> Int
applyKernel img kernel x y = sum [getPixel img (x + dx) (y + dy) * (round k) | (dx, dy, k) <- kernelElements]
  where
    kernelElements = [(dx, dy, kernelValue kernel dx dy) | dx <- [-kw..kw], dy <- [-kh..kh]]
    kw = width kernel `div` 2
    kh = height kernel `div` 2

-- カーネルの値を取得する
-- カーネルの範囲外の座標に対しては、最も近い画素の値を使う
kernelValue :: Kernel -> Int -> Int -> Double
kernelValue kernel dx dy = (dat kernel !! (dy + kh)) !! (dx + kw)
  where
    kw = width kernel `div` 2
    kh = height kernel `div` 2

-- 画像データの指定座標の画素値を取得する
getPixel :: Image -> Int -> Int -> Int
getPixel img x y = dat img !! (clamp y 0 (height img - 1)) !! (clamp x 0 (width img - 1))

-- 画素値を0から255の範囲に収める
clamp :: Int -> Int -> Int -> Int
clamp v minVal maxVal = max minVal (min maxVal v)


{-
Process the given image data by multiplying each pixel by the values of the given weights matrix.

Parameters

*imageData*: a flat (one-dimensional) array of the image data. The data is organized by row and then by column, and then one byte for each color channel red, green, and blue. These values will always be integers in the range 0-255.
For example, given the image:
A B C
D E F
G H I
The input array would be:

[
  A_Red, A_Green, A_Blue, B_Red, B_Green, B_Blue, C_Red, C_Green, C_Blue, 
  D_Red, D_Green, D_Blue, E_Red, E_Green, E_Blue, F_Red, F_Green, F_Blue,
  G_Red, G_Green, G_Blue, H_Red, H_Green, H_Blue, I_Red, I_Green, I_Blue
]
height: the number of rows of the image.
width: the number of columns of the image.
weights: an n x n array giving the weights for each of the neighboring pixels. The size of this array, n, will always be odd, with the center being the weight of the pixel itself. The array is by row and then column. I.e. weights[y][x]

Return

An array of the image data adjusted by the weighted average per pixel's neighborhood. Where the weights matrix specifies pixels outside the actual image, use the values of the closest pixel. (E.g. extend the edges as far as necessary to provide values for the matrix.) Each value should be in the range 0-255.
For more information about image processing please see wikipedia

Tools

A few helpful functions have been set up for you.
This function / variables returns the codewars logo, height and width:

codewars().rgb, codewars().height, codewars().width
Displays an image in the console output. Click the image to view the image source :

putImageData( imageData, height, width, displayHeight, displayWidth );
This function returns several pre-defined weights matrices. Try blur, sharpen, edgeDetect, gaussianBlur, and laplacianFilter :

kernals().blur
Returns true if actual is within the acceptable error margin of expected. Some of the tests could be prone to floating point errors, so this function allows for +/- 3 (about 1%) of the expected value :

assertRGBImageDataSimilar( actual, expected, height, width )
Compares your solution to the expected solution and then provides a nice display of the original, your result, and the expected result. You can click any of these images to view the source data :

imageTest( imageData, h, w, weights, expected )

-}