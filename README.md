# NARP (N-Area Reliability Program) 
(Fortran version)

![64794652](https://user-images.githubusercontent.com/19656104/213796836-fc5851a8-5f60-496e-9e51-7b6c80e065f5.png)<img width="640" alt="DrSinghHead" src="https://user-images.githubusercontent.com/19656104/213797109-626ab421-f3e5-4d8c-920e-b7849959bc9b.PNG">

## Origin
**NARP** is a Fortran program originally developed by [Prof. Chanan Singh](https://engineering.tamu.edu/electrical/profiles/csingh.html) (Texas A&M University) and the Associated Power Analysts, Inc. to perform bulk power system reliability assessment.
(This github repository is maintianed by Prof. Singh's postdoc [Dr. Yongli Zhu](https://yonglizhu.github.io/#research-interest))

## Main Features
Here are a few features of the **NARP** tool:
* Sequential Monte Carlo simulation
* Unit preventive maintenance schedule
* Consider transmission model and optimal power-flow-based load shedding
* Offer both 3- and 2-state generator models	
* Provide simulation-restart capability (from saved snapshot files)
* Support the "Load loss sharing" mode

A detailed tutorial can be found on our doc [NARP_USER_GUIDE](https://github.com/zylpascal/NARP/blob/main/NARP-USERGUIDE%20copy.pdf).


## Where to get it
* Clone or Fork the source code on [GitHub](https://github.com/zylpascal/NARP). 
* Or, get a binary file with GUI released from the maintainer [Dr. Yongli Zhu](https://yonglizhu.github.io/#research-interest)


## Installation

1. Install Fortran compiler (skip this step if you already have a proper compiler)

   *To maximize the speed advantage of this NARP tool, the Intel Fortran compiler is recommended, which is free to academic users.*

2. Install a proper code editor, e.g., Visual Studio Code

3. Git clone or Download the code zip file, unzip it and navigate to the unzipped folder, 

4. Open the NARP.FOR source file, compile and build the source code file in the command line or an IDE (integrated development environment)
(e.g., in the Visual Studio IDE, you can just click the "Build" button)
   
5. The compiling/building  may take a while; after successfully built, a NARP.exe file will be generated at the "bin\Debug" or "bin\Release" subfolder
(for quick debugging, the simplest way is to directly click the Build and Run button in Visual Studio IDE)

## License
[MIT](LICENSE)


## Documentation
[Code documentation][docstrings] inline docstrings are provided in the source code file.
Besides, 
1)	A [NARP_USER_GUIDE](https://github.com/zylpascal/NARP/blob/main/NARP-USERGUIDE%20copy.pdf) pdf file is provided with more details regarding the background logic , input/output formats, and examples.
2)	A related [Sandia Report](https://github.com/zylpascal/NARP/blob/main/Sandia.pdf) is also uploaded in this repository, where real use cases of this NARP tool were reported.


## Communication Channels 
TBD. (email list or Slack workspace to get in touch with us.)


## Contributing
All contributions (bug report, documentation, feature development, etc.) are welcome. A guide on how to contribute to this project will be announced soon (e.g., formatting conventions).

## No Warranty Disclaimer
Software NARP is distributed free of cost for educational and research purposes, and there is no warranty for the program. The users use the program at their own risk. No support for this program will be provided. The source code is provided so that users can modify and extend the program for their individual needs and the benefit of society.


