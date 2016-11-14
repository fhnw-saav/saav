package ch.fhnw.ima.saav

/*
	jsPDF scala wrapper for MrRio jsPDF lib (https://github.com/MrRio/jsPDF)
	by François Nicaise (contact@francoisnicaise.fr)

	License
	(MIT License)
	Copyright (c) 2015-End of world François Nicaise, https://bitbucket.org/fnicaise/scalajspdf
	Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
	The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
package jspdf

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

/**
  *
  * @constructor Creates new jsPDF document object instance.
  * @param orientation One of "portrait" or "landscape" (or shortcuts "p" (Default), "l")
  * @param unit        Measurement unit to be used when coordinates are specified.
  *                    One of "pt" (points), "mm" (Default), "cm", "in"
  * @param format      One of 'pageFormats' as shown below, default: a4
  */
@js.native
@JSName("jsPDF")
class jsPDF( orientation : String = "i", unit : String = "mm", format : String = "a4") extends js.Object {

  /**
    * Add a custom font.
    *
    * @param postscript name of the Font.  Example: "Menlo-Regular"
    * @param name of font-family from @font-face definition.  Example: "Menlo Regular"
    * @param font style.  Example: "normal"
    * @return the jsPDF updated object
    */
  def addFont( postscript : String, name : String, font : String ) : jsPDF = js.native

  /**
    * Adds (and transfers the focus to) new page to the PDF document.
    * @return the jsPDF updated object
    */
  def addPage() : jsPDF = js.native

  /**
    * Adds a circle to PDF
    *
    * @param x Coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @param r Radius (in units declared at inception of PDF document)
    * @param style A string specifying the painting style or null.  Valid styles include: 'S' [default] - stroke, 'F' - fill,  and 'DF' (or 'FD') -  fill then stroke. A null value postpones setting the style so that a shape may be composed using multiple method calls. The last drawing method call used to define the shape should not have a null style argument.
    * @return the jsPDF updated object
    */
  def circle(x : Int , y: Int, r: Int, style: String = "S") : jsPDF = js.native

  /**
    * Adds an ellipse to PDF
    *
    * @param x Coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @param rx Radius along x axis (in units declared at inception of PDF document)
    * @param ry Radius along y axis (in units declared at inception of PDF document)
    * @param style A string specifying the painting style or null.  Valid styles include: 'S' [default] - stroke, 'F' - fill,  and 'DF' (or 'FD') -  fill then stroke. A null value postpones setting the style so that a shape may be composed using multiple method calls. The last drawing method call used to define the shape should not have a null style argument.
    * @return the jsPDF updated object
    */
  def ellipse(x : Int, y: Int, rx : Int, ry: Int, style: String = "S") : jsPDF = js.native

  /**
    * Returns an object - a tree of fontName to fontStyle relationships available to
    * active PDF document.
    * @return Like {'times':['normal', 'italic', ... ], 'arial':['normal', 'bold', ... ], ... }
    */
  def getFontList() : js.Object = js.native

  /**
    * Adds series of curves (straight lines or cubic bezier curves) to canvas, starting at `x`, `y` coordinates.
    * All data points in `lines` are relative to last line origin.
    * `x`, `y` become x1,y1 for first line / curve in the set.
    * For lines you only need to specify [x2, y2] - (ending point) vector against x1, y1 starting point.
    * For bezier curves you need to specify [x2,y2,x3,y3,x4,y4] - vectors to control points 1, 2, ending point. All vectors are against the start of the curve - x1,y1.
    *
    * @note lines([[2,2],[-2,2],[1,1,2,2,3,3],[2,1]], 212,110, 10) // line, line, bezier curve, line
    * @param lines Array of *vector* shifts as pairs (lines) or sextets (cubic bezier curves).
    * @param x Coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @param scale (Defaults to [1.0,1.0]) x,y Scaling factor for all vectors. Elements can be any floating number Sub-one makes drawing smaller. Over-one grows the drawing. Negative flips the direction.
    * @param style A string specifying the painting style or null.  Valid styles include: 'S' [default] - stroke, 'F' - fill,  and 'DF' (or 'FD') -  fill then stroke. A null value postpones setting the style so that a shape may be composed using multiple method calls. The last drawing method call used to define the shape should not have a null style argument.
    * @param closed If true, the path is closed with a straight line from the end of the last curve to the starting point.
    * @return the jsPDF updated object
    */
  def lines(lines : js.Array[js.Array[Double]], x : Double = 0, y : Double = 0, scale : Double = 1.0, style : String = "S", closed: Boolean = false) : jsPDF = js.native

  /**
    * Draws a line between two points
    *
    * @param x1 First point coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y1 First point Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @param x2 Second point coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y2 Second point Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @return the jsPDF updated object
    */
  def line( x1 : Double, y1 : Double, x2 : Double, y2 : Double ) : jsPDF = js.native


  /**
    * Generates the PDF document.
    *
    * If `type` argument is undefined, output is raw body of resulting PDF returned as a string.
    *
    * @param ttype A string identifying one of the possible output types.
    * @param options An object providing some additional signalling to PDF generator.
    * @return the jsPDF updated object
    */
  def output(ttype : String, options : js.Any ) : jsPDF = js.native

  /**
    * Generates the PDF document.
    *
    * If `type` argument is undefined, output is raw body of resulting PDF returned as a string.
    *
    * @param ttype A string identifying one of the possible output types.
    * @return the output String
    */
  def output(ttype : String ) : String = js.native

  /**
    * Adds a rectangle to PDF
    *
    * @param x Coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @param w Width (in units declared at inception of PDF document)
    * @param h Height (in units declared at inception of PDF document)
    * @param style A string specifying the painting style or null.  Valid styles include: 'S' [default] - stroke, 'F' - fill,  and 'DF' (or 'FD') -  fill then stroke. A null value postpones setting the style so that a shape may be composed using multiple method calls. The last drawing method call used to define the shape should not have a null style argument.
    * @return the jsPDF updated object
    */
  def rect(x : Int, y: Int, w: Int, h: Int, style : String = "S" ) : jsPDF = js.native

  /**
    * Adds a rectangle with rounded corners to PDF
    *
    * @param x Coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @param w Width (in units declared at inception of PDF document)
    * @param h Height (in units declared at inception of PDF document)
    * @param rx Radius along x axis (in units declared at inception of PDF document)
    * @param ry Radius along y axis (in units declared at inception of PDF document)
    * @param style A string specifying the painting style or null.  Valid styles include: 'S' [default] - stroke, 'F' - fill,  and 'DF' (or 'FD') -  fill then stroke. A null value postpones setting the style so that a shape may be composed using multiple method calls. The last drawing method call used to define the shape should not have a null style argument.
    * @return the jsPDF updated object
    */
  def roundedRect(x:Int, y:Int, w:Int, h:Int, rx:Int, ry:Int, style : String = "S" ) : jsPDF = js.native

  /**
    * Saves as PDF document. An alias of jsPDF.output('save', 'filename.pdf')
    * @param  name The filename including extension.
    * @return the jsPDF updated object
    */
  def save( name : String ) : jsPDF = js.native

  /**
    * Sets the stroke color for upcoming elements.
    *
    * Depending on the number of arguments given, Gray, RGB, or CMYK
    * color space is implied.
    *
    * When only ch1 is given, "Gray" color space is implied and it
    * must be a value in the range from 0.00 (solid black) to to 1.00 (white)
    * if values are communicated as String types, or in range from 0 (black)
    * to 255 (white) if communicated as Number type.
    * The RGB-like 0-255 range is provided for backward compatibility.
    *
    * When only ch1,ch2,ch3 are given, "RGB" color space is implied and each
    * value must be in the range from 0.00 (minimum intensity) to to 1.00
    * (max intensity) if values are communicated as String types, or
    * from 0 (min intensity) to to 255 (max intensity) if values are communicated
    * as Number types.
    * The RGB-like 0-255 range is provided for backward compatibility.
    *
    * When ch1,ch2,ch3,ch4 are given, "CMYK" color space is implied and each
    * value must be a in the range from 0.00 (0% concentration) to to
    * 1.00 (100% concentration)
    *
    * Because JavaScript treats fixed point numbers badly (rounds to
    * floating point nearest to binary representation) it is highly advised to
    * communicate the fractional numbers as String types, not JavaScript Number type.
    *
    * @param ch1 Color channel value
    * @return the jsPDF updated object
    */
  // Grayscale
  def setDrawColor(ch1 : Int) : jsPDF = js.native
  def setDrawColor(ch1 : String) : jsPDF = js.native

  // RGB
  def setDrawColor(ch1 : Int, ch2 : Int, ch3 : Int ) : jsPDF = js.native
  def setDrawColor(ch1 : String, ch2 : String, ch3 : String) : jsPDF = js.native

  // CMYK
  def setDrawColor(ch1 : Int, ch2 : Int, ch3 : Int, ch4 : Int) : jsPDF = js.native
  def setDrawColor(ch1 : String, ch2 : String, ch3 : String, ch4 : String) : jsPDF = js.native

  /**
    * Sets the fill color for upcoming elements.
    *
    * Depending on the number of arguments given, Gray, RGB, or CMYK
    * color space is implied.
    *
    * When only ch1 is given, "Gray" color space is implied and it
    * must be a value in the range from 0.00 (solid black) to to 1.00 (white)
    * if values are communicated as String types, or in range from 0 (black)
    * to 255 (white) if communicated as Number type.
    * The RGB-like 0-255 range is provided for backward compatibility.
    *
    * When only ch1,ch2,ch3 are given, "RGB" color space is implied and each
    * value must be in the range from 0.00 (minimum intensity) to to 1.00
    * (max intensity) if values are communicated as String types, or
    * from 0 (min intensity) to to 255 (max intensity) if values are communicated
    * as Number types.
    * The RGB-like 0-255 range is provided for backward compatibility.
    *
    * When ch1,ch2,ch3,ch4 are given, "CMYK" color space is implied and each
    * value must be a in the range from 0.00 (0% concentration) to to
    * 1.00 (100% concentration)
    *
    * Because JavaScript treats fixed point numbers badly (rounds to
    * floating point nearest to binary representation) it is highly advised to
    * communicate the fractional numbers as String types, not JavaScript Number type.
    *
    * @param ch1 Color channel value
    * @return the jsPDF updated object
    */
  // Grayscale
  def setFillColor(ch1 : Int) : jsPDF = js.native
  def setFillColor(ch1 : String) : jsPDF = js.native

  // RGB
  def setFillColor(ch1 : Int, ch2 : Int, ch3 : Int ) : jsPDF = js.native
  def setFillColor(ch1 : String, ch2 : String, ch3 : String) : jsPDF = js.native

  // CMYK
  def setFillColor(ch1 : Int, ch2 : Int, ch3 : Int, ch4 : Int) : jsPDF = js.native
  def setFillColor(ch1 : String, ch2 : String, ch3 : String, ch4 : String) : jsPDF = js.native

  /**
    * Sets text font face, variant for upcoming text elements.
    * See output of jsPDF.getFontList() for possible font names, styles.
    *
    * @param fontName Font name or family. Example: "times"
    * @param fontStyle Font style or variant. Example: "italic"
    * @return the jsPDF updated object
    */
  def setFont(fontName: String, fontStyle: String): jsPDF = js.native

  /**
    * Sets font size for upcoming text elements.
    *
    * @param size Font size in points.
    * @return the jsPDF updated object
    */
  def setFontSize(size:Int) : jsPDF = js.native

  /**
    * Sets the line cap styles
    * See {jsPDF.CapJoinStyles} for variants
    *
    * @param style A number identifying the type of line cap
    * @return the jsPDF updated object
    */
  def setLineCap(style : Int) : jsPDF = js.native

  /**
    * Sets the line cap styles
    * See {jsPDF.CapJoinStyles} for variants
    *
    * @param style A string identifying the type of line cap
    * @return the jsPDF updated object
    */
  def setLineCap(style : String) : jsPDF = js.native

  /**
    * Sets the line join styles
    * See {jsPDF.CapJoinStyles} for variants
    *
    * @param style A number identifying the type of line join
    * @return the jsPDF updated object
    */
  def setLineJoin(style : Int ) : jsPDF = js.native

  /**
    * Sets the line join styles
    * See {jsPDF.CapJoinStyles} for variants
    *
    * @param style A string identifying the type of line join
    * @return the jsPDF updated object
    */
  def setLineJoin(style : String ) : jsPDF = js.native

  /**
    * Sets line width for upcoming lines.
    *
    * @param width Line width (in units declared at inception of PDF document)
    */
  def setLineWidth(width:Double): jsPDF = js.native

  /**
    * Adds a properties to the PDF document
    *
    * @param A property_name-to-property_value object structure.
    * @return the jsPDF updated object
    */
  def setProperties(A: js.Object )  : jsPDF = js.native

  /**
    * Sets the text color for upcoming elements.
    * If only one, first argument is given,
    * treats the value as gray-scale color value.
    *
    * @param r Red channel color value in range 0-255 or {String} r color value in hexadecimal, example: '#FFFFFF'
    * @param g Green channel color value in range 0-255
    * @param b Blue channel color value in range 0-255
    * @return the jsPDF updated object
    */
  def setTextColor(r:Int, g:Int, b:Int) : jsPDF = js.native

  /**
    * Sets the text color for upcoming elements.
    * treats the value as gray-scale color value.
    *
    * @param r Red channel color value in range 0-255 or {String} r color value in hexadecimal, example: '#FFFFFF'
    * @return the jsPDF updated object
    */
  def setTextColor(r:Int) : jsPDF = js.native

  /**
    * Splits a given string into an array of strings. Uses 'size' value
    * (in measurement units declared as default for the jsPDF instance)
    * and the font's "widths" and "Kerning" tables, where availabe, to
    * determine display length of a given string for a given font.
    * We use character's 100% of unit size (height) as width when Width
    * table or other default width is not available.
    *
    * @param text Unencoded, regular JavaScript (Unicode, UTF-16 / UCS-2) string.
    * @param size Nominal number, measured in units default to this instance of jsPDF.
    * @param options Optional flags needed for chopper to do the right thing.
    * @return array with strings chopped to size.
    */
  def splitTextToSize( text : String, size: Double, options : js.Any) : js.Array[String] = js.native


  /**
    * Adds text to page. Supports adding multiline text when 'text' argument is an Array of Strings.
    *
    * @param text String to be added to the page. Each line is shifted one line down per font, spacing settings declared before this call.
    * @param x Coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @param flags Collection of settings signalling how the text must be encoded. Defaults are sane. If you think you want to pass some flags, you likely can read the source.
    * @return the jsPDF updated object
    */
  def text(text : String, x: Double, y : Double, flags : js.Any ) : jsPDF = js.native

  /**
    * Adds text to page. Supports adding multiline text when 'text' argument is an Array of Strings.
    *
    * @param text String to be added to the page. Each line is shifted one line down per font, spacing settings declared before this call.
    * @param x Coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @return the jsPDF updated object
    */
  def text(text : String, x: Double, y : Double ) : jsPDF = js.native

  /**
    * Adds text to page. Supports adding multiline text when 'text' argument is an Array of Strings.
    *
    * @param text Array of String to be added to the page. Each line is shifted one line down per font, spacing settings declared before this call.
    * @param x Coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @param flags Collection of settings signalling how the text must be encoded. Defaults are sane. If you think you want to pass some flags, you likely can read the source.
    * @return the jsPDF updated object
    */
  def text(text : js.Array[String], x: Double, y : Double, flags : js.Any ) : jsPDF = js.native

  /**
    * Adds text to page. Supports adding multiline text when 'text' argument is an Array of Strings.
    *
    * @param text Array of String to be added to the page. Each line is shifted one line down per font, spacing settings declared before this call.
    * @param x Coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @return the jsPDF updated object
    */
  def text(text : js.Array[String], x: Double, y : Double ) : jsPDF = js.native

  /**
    * Adds a triangle to PDF
    *
    * @param x1 Coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y1 Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @param x2 Coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y2 Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @param x3 Coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y3 Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @param style A string specifying the painting style or null.  Valid styles include: 'S' [default] - stroke, 'F' - fill,  and 'DF' (or 'FD') -  fill then stroke. A null value postpones setting the style so that a shape may be composed using multiple method calls. The last drawing method call used to define the shape should not have a null style argument.
    * @return the jsPDF updated object
    */
  def triangle(x1 : Int, y1 : Int, x2 : Int, y2 : Int, x3 : Int, y3 : Int, style: String = "S"): jsPDF = js.native

  /* -------------------- JSPDF PLUGINS -----------------------*/

  /**
    * Adds an image to document
    *
    * @param imageData String of base64 encoded image
    * @param format ('jpeg', 'png')
    * @param x Coordinate (in units declared at inception of PDF document) against left edge of the page
    * @param y Coordinate (in units declared at inception of PDF document) against upper edge of the page
    * @param w width (in units declared at inception of PDF document)
    * @param h height (in units declared at inception of PDF document)
    * @param alias ??
    * @param rotation rotation angle
    * @return the jsPDF updated object
    */
  def addImage(imageData : String, format : String = "string", x : Int, y : Int, w : Double, h: Double, alias : String = "alias", compression : String = "alias", rotation : Double = 0.0 ) : jsPDF = js.native

  /**
    * write HTML content to pdf
    * @param HTML pointer to DOM element that is to be rendered into PDF.
    * @param x starting X coordinate in jsPDF instance's declared units.
    * @param y starting Y coordinate in jsPDF instance's declared units.
    * @param settings Additional / optional variables controlling parsing, rendering.
    * @return the jsPDF instance
    */
  def fromHTML (HTML : js.Object, x : Double = 0.0, y: Double = 0.0, settings : js.Object = Nil) : jsPDF = js.native

  /**
    * Renders an HTML element to canvas object which added to the PDF
    *
    * This PlugIn requires html2canvas: https://github.com/niklasvh/html2canvas
    *            OR rasterizeHTML: https://github.com/cburgmer/rasterizeHTML.js
    *
    * NOTE. scalajs users: rasterizeHTML has already been included as a one JS file (in sr/main/resources) so no need to add this dependency
    *
    * @param element HTML String, or anything supported by html2canvas.
    * @param x starting X coordinate in jsPDF instance's declared units.
    * @param y starting Y coordinate in jsPDF instance's declared units.
    * @param options Additional options, check the code below.
    * @param callback to call when the rendering has finished.
    *
    * NOTE: Every parameter is optional except 'element' and 'callback', in such
    *       case the image is positioned at 0x0 covering the whole PDF document
    *       size. Ie, to easily take screenshots of webpages saving them to PDF.
    */
  def addHTML (element : String, x : Int, y : Int, options : js.Object , callback : (Any*) => Unit) : jsPDF = js.native

}
