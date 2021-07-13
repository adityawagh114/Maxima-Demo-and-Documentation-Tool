# Maxima-Demo-and-Documentation-Tool.

The purpose of this project is to create functions which convert a wxMaxima worksheet to Texinfo. The functions to convert a worksheet will be written in Common Lisp, which is the implementation language for Maxima itself.


# wxmx2texi

## Steps to run
- Download this package and save in the maxima directory such that maxima can find this package.
- Load the package by executing the command load(wxmx2texi) in maxima. 
- Pass the location of the xml file as an argument to wxmx2texi() function.
- To save the output in the texinfo file, pass a second optional argument of the loaction to wxmx2texi() function.  


## Required dependencies 
- [QuickLisp](https://www.quicklisp.org/beta/) to Load the XML Parser and Zip file Manager.
