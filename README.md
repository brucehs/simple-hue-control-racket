# simple-hue-control-racket
Simple Program to Control Philip Hue Lights in a Theatrical Setting. Currently written in Racket.  

## Overview

This program was originally created for [Superhero Clubhouse's](http://www.superheroclubhouse.org) 2015-07 workshop of [*Earth*](http://www.superheroclubhouse.org/earth/). The project took place in a studio setting, without a theatrical lighting system, and half the play was created by the audience during the event. In order to collaborate with the ensemble in real time, I used [Philip Hue Bulbs](http://www2.meethue.com/en-us/) as they provide wireless control and color changing ability. I am providing the code of the program I created for those who wish to produce their own production of *Earth* from the Atlas or for those who want to experiment with other theatrical uses of these bulbs.  

## Notes

I am only providing the source code and not a compiled binary as there are still hard coded aspects (such as the user name) that will need to be changed for the program to be functional. A free download of [Dr. Racket](http://download.racket-lang.org/) is needed to run or compile the software. Moreover, familiarity with the [Philip Hue API](http://www.developers.meethue.com/philips-hue-api) is highly recommended.  

**Some Limitations:** 

* There is currently no way to patch lights, so the bulbs must be controlled in the order that they have been registered with the Hue Bridge.  

* The Bridge Address and Bridge User Name must be set before the program will function. They are available in the Bridge Menu that appears when the main, "Simple Hue Control", window is in the foreground. Currently, there is no error handling and the program will crash if it is used before these values are set.  

* Everything runs on the main thread, so larger RESTful commands freeze the program as they are executed.  

* For theatrical purposes, a dedicated router—not connected to the internet—is recommended.  

* I've only run the program on Mac OS X 10.9 and 10.10 and Racket v 6.0. I have no clue how it will function on other platforms. As it stores configuration files in ~/Library/Application Support/Simple Hue Control/, there will probably be problems. Adding support for Windows and Linux is a low priority, so I suggest forking if such functionality is important to you.  

## How To  

*Under Construction*  

## Screen Shots

![Main Window](https://github.com/brucehs/simple-hue-control-racket/blob/master/images/main_window.tiff)  
*Main Window*  

![Last Status](https://github.com/brucehs/simple-hue-control-racket/blob/master/images/last_status.tiff)  
*Last Lighting Change*  

![Current Light Status](https://github.com/brucehs/simple-hue-control-racket/blob/master/images/all_lights.tiff)
*Current Light Status*  

![Cue List](https://github.com/brucehs/simple-hue-control-racket/blob/master/images/cue_list.tiff)  
*Cue List*
