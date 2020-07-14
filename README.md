## FUN_imageCompressor_2019
Code an image Compressor in Haskell using Kmeans Algorithm

## How to use

``` make```
This will compile an executable called ```imageCompressor```.

### Arguments
./imageCompressor n e IN
  
  . n   number of colors in the final image
  . e   convergence limit
  . IN  path to the file containing the colors of the pixel

Example :
```./imageCompressor 3 0.8 liste ``` will compress the image contained in the IN file with 3 final colors and a convergence limit of 0.8.

![alt text](https://i.imgur.com/WKZIl5B.png)

## Authors

* **Niels Hoez** - [nielshoez](https://github.com/nielshoez)


## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details
