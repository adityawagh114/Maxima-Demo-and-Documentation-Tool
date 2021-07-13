
# wxmx2texi

## Required dependencies 
- [QuickLisp](https://www.quicklisp.org/beta/) to Load the XML Parser and Zip file Manager.

## How to install Quicklisp in Maxima.
- Download [quicklisp.lisp](https://beta.quicklisp.org/quicklisp.lisp) and save it in maxima.
- Load the file in maxima by executing ```load("quicklisp.lisp")```.
- Install quicklisp by executing ```:lisp (quicklisp-quickstart:install)```


## Steps to run
- Download this package and save in the maxima directory such that maxima can find this package.
- Load the package by executing the command ```load(wxmx2texi)``` in maxima. 
- Pass the location of the xml file as an argument to wxmx2texi() function.
- To save the output in the texinfo file, pass a second optional argument of the loaction to ```wxmx2texi()``` function.  

