# WHO EML storyboard 2016

This repository contains the files required to generate the **'Neuropathic pain: so many people, but where are the drugs?** storyboard. 

The storyboard was created by the Essential Medicines Working Group of the [International Association for the Study of Pain](http://iasp-pain.org) (IASP), in collaboration with the Neuropathic Pain Special Interest Group of IASP (NeuPSIG), and the [International Association for Hospice and Palliative Care](http://hospicecare.com/home/) (IAHPC). The working group was tasked with developing an application for the inclusion of the alpha-2-delta calcium channel ligand, gabapentin, in the 2017 update of the WHO Model Lists of Essential Medicines [(WHO EML)](http://www.who.int/medicines/publications/essentialmedicines/en/). 

Working group members included:
- Peter R Kamerman (South Africa, Chair) 
- Nanna B Finnerup (Denmark, IASP/NeuPSIG) 
- Liliana De Lima (USA, IAHPC)
- Simon Haroutounian (USA, IASP) 
- Srinivasa N Raja (USA, IASP/NeuPSIG)
- Andrew SC Rice (UK, IASP/NeuPSIG)
- Blair H Smith (UK, IASP/NeuPSIG)
- Rolf-Detlef Treede (Germany, IASP)

## Generating the storyboard

Follow the steps below to generate the application, appendices, and the executive summary.

#### If you use Git/GitHub: 
1. _Fork_ the repository to your GitHub account 

2. _Clone_ the repository to your computer 

3. Open a _terminal_ (Windows users should open a _Git Bash_ terminal) and change the path to the root directory of the respository  

4. Type _'make'_ (Windows users will need to download and install [_GNU Make_](http://gnuwin32.sourceforge.net/downlinks/make.php) beforehand)  

 
#### If you do not use Git/GitHub:
1. Windows users must download and install:
    - [_Git for Windows_](https://github.com/git-for-windows/git/releases) or any other _Bash_-like shell for Windows
    - [_GNU Make_](http://gnuwin32.sourceforge.net/downlinks/make.php)

2. _Download_ the respository as a zip file 

2. _Unzip_ the repository on your computer 

3. Open a _terminal_ and change the path to the root directory of the respository

4. Type _'make'_

Please note that to limit the file size, the ouput has been set in the YAML header of _index.Rmd_ to: `self_contained: false`, and document dependencies (other than images and data) are stored remotely `lib_dir: 'https://dl.dropboxusercontent.com/u/11805474/painblogr/html-head'`. Please change these settings as you require. 

## The following set-up was used to generate the storyboard
- R version 3.2.4 (2016-03-10) running on RStudio v0.99.1243 for OSX
- Packages used (inclusive of: _index.Rmd_ and all analysis scripts):
  - sp_1.2-3          
  - geojsonio_0.2.0   
  - stringr_1.1.0     
  - boot_1.3-18 
  - knitr_1.14        
  - leaflet_1.0.1     
  - scales_0.4.0      
  - viridis_0.3.4    
  - ggiraph_0.3.1     
  - ggplot2_2.1.0     
  - readr_1.0.0       
  - tidyr_0.6.0
  - tibble_1.2        
  - dplyr_0.5.0     
  - flexdashboard_0.3
  
## Queries and issues
If you have any queries or issues, please log an [issue](https://github.com/kamermanpr/WHO-EML-storyboard-2016/issues). 
