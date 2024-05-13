# IMBA: Individual Maggot Behavior Analyzer


## Introduction <a name="introduction"></a>

Neuronally orchestrated muscular movement and locomotion are fundamental aspects of multicellular animals. The larva of the fruit fly Drosophila melanogaster provides a unique opportunity to study these processes due to its simple brain and genetic accessibility. However, traditional methods of studying larval locomotion often aggregate measurements across animals or test animals individually, limiting our ability to understand inter- and intra-individual variability in locomotion and its neurogenetic determinants.

To address this gap, we introduce the Individual Maggot Behavior Analyzer (IMBA), a software tool designed for analyzing the behavior of individual larvae within groups. IMBA enables researchers to reliably resolve individual larval identities across collisions, providing unprecedented insights into locomotion variability and its underlying genetic and neural mechanisms.

In this README, we provide an overview of IMBA's features, installation instructions, usage guidelines, and examples of its application in studying larval behavior in various biomedical research contexts. With IMBA, researchers can obtain a rich understanding of individual larval behavior, paving the way for deeper insights into neurogenetic pathways governing locomotion and its modulation.

## Table of Contents
1. [Introduction](#introduction)
2. [IMBAtracker](#imbatracker)
3. [IMBAvisualizer](#imbavisualizer)

---

# IMBAtracker <a name="imbatracker"></a>
The IMBAtracker consists of mainly two parts. The first part is the Tracking software that is written in c++. It is developed with the help of the OpenCV library and CVblob. In order to run the tracker this c++ software has to be installed. It can be used as a standalone tool via command line.
The second part is the GUI which is developed in Python 3.9 using the Qt library. The GUI is providing an easy use of the command line tool for tracking large amounts of data. First we will install the Tracker and then the GUI.

## Installation - Tracker

### Cloning Repository and using WSL
If you are using Ubuntu you can skip step 1 and 2.
1. Open command prompt and install WSL2 with Ubuntu 18.04 using:
```
wsl --install -d Ubuntu-18.04
```

This will open another command prompt with the Ubuntu-18.04 system. Here do the following:
2. Enter desired user name and password...
3. Install git and clone this repository:
```
sudo apt update
sudo apt install git
sudo git clone https://github.com/mthane/IMBA
```
### Docker Image
Because of older dependencies which have caused issues we will simply use a Docker Image that encapsulates our c++ software. Next we explain how to install Docker, use our c++ Tracking Software in the command line, and give some examples. Docker is a platform that allows you to develop, ship, and run applications in containers. Here's how to install Docker on a Windows system:

#### Prerequisites
- Windows 10 64-bit: Pro, Enterprise, or Education edition
- Hardware virtualization support enabled in BIOS settings

#### Steps
1. **Download Docker Desktop**: Visit the [Docker Desktop for Windows](https://www.docker.com/products/docker-desktop) webpage and download the Docker Desktop installer.

2. **Install Docker Desktop**: Double-click the downloaded installer (Docker Desktop Installer.exe) to start the installation process. Follow the on-screen instructions to complete the installation.

3. **Enable Hyper-V**: During installation, Docker Desktop will prompt you to enable Hyper-V and Windows Subsystem for Linux (WSL). Click "Install" to enable these features. Note that enabling Hyper-V requires a system restart.

4. **Start Docker Desktop**: Once installation is complete and your system has restarted, Docker Desktop should start automatically. You'll see the Docker icon in the system tray.

5. **Verify Installation**: Open a command prompt or PowerShell window and run the following command to verify that Docker is installed correctly:
```
docker --version
```

#### Build Docker Image

```
cd IMBA/IMBAtracker
bash setup.sh
```

### Usage 


### Examples

## Installation - GUI

### Usage 


### Examples

# IMBAvisualizer <a name="imbavisualizer"></a>

Install Rtools 4.2 form https://cran.r-project.org/bin/windows/Rtools/
```
cd IMBA/IMBAvisualizer
sudo bash setup.sh
```


