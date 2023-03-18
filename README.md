<p align="center">
  <a href="" rel="noopener">
 <img width=200px height=200px src="https://i.imgur.com/6wj0hh6.jpg" alt="Project logo"></a>
</p>

<h3 align="center">VSynth</h3>

<div align="center">

[![Status](https://img.shields.io/badge/status-active-success.svg)]()
[![GitHub Issues](https://img.shields.io/github/issues/ex3c-dev/vsynth)](https://github.com/ex3c-dev/vsynth/issues)
[![License](https://img.shields.io/github/license/ex3c-dev/vsynth)](/LICENSE)

</div>

---

<p align="center"> A basic digital audio workstation (DAW) to create music using algorithms written in haskell.
    <br> 
</p>

## 📝 Table of Contents

- [About](#about)
- [Getting Started](#getting_started)
- [Built Using](#built_using)
- [Authors](#authors)
- [Acknowledgments](#acknowledgement)

## 🧐 About <a name = "about"></a>

This project is part of the evaluation for the module FFP at Ludwig Maximilian University of Munich.

## 🏁 Getting Started <a name = "getting_started"></a>

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See [deployment](#deployment) for notes on how to deploy the project on a live system.

1. Clone the project repository.
2. Build the executable:
   - On Windows:
     - Open a powershell and navigate to the project directory
     - Set the execution policy by typing the following command and pressing A to accept all: "Set-ExecutionPolicy RemoteSigned"
     - Execute the powershell script by typing the following command: ".\\build_windows.ps1"   
   - On Linux:
     - Just execute: "stack build"
3. Run the project by typing: "stack exec vsynth-exe"

### Prerequisites

What things you need to install the software and how to install them.

```
The Glasgow Haskell Compiler - ghc
Stack
```

## ⛏️ Built Using <a name = "built_using"></a>

- [OpenAL](https://hackage.haskell.org/package/OpenAL) - Haskell binding for the OpenAL cross-platform 3D audio API
- [GHC](https://www.haskell.org/ghc/) - The Glasgow Haskell Compiler

## ✍️ Authors <a name = "authors"></a>

- [@ex3c-dev](https://github.com/ex3c-dev) - Idea & Initial work
- [@powerkarrot](https://github.com/powerkarrot) - Idea & Initial work

## 🎉 Acknowledgements <a name = "acknowledgement"></a>

- Thanks to [@svenpanne](https://github.com/svenpanne) for the haskell bindings
