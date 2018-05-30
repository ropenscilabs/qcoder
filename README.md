# QCoder
Lightweight package to conduct qualitative coding  
<p><img src="hex/imgHex.png", width="150", align="right" /></p>

## Installation

To install the latest development version, run

```r
install.packages("devtools")
devtools::install_github("ropenscilabs/qcoder")
```

## Motivation

The motivation stems from the need for a free, open source option for analyzing textual qualitative data. Qualitative data refers to text from interview transcripts, observation notes, memos, and primary source/archival documents. 

Qualitative data analysis (QDA) processes, particularly those developed by Corbin and Strauss  (2014), Miles, Huberman, and Saldana (2013), and Glaser and Strauss (2017), can be thought of as layering interpretation onto the text. The researcher starts with open coding, meaning that she is free to tag snippets of text with whatever descriptions she deems appropriate. For instance, if the researcher is coding observation notes and senses that conversations between two individuals will be relevant to the research questions, she might tag the instances in which the two individuals speak with the code "conversation." In the next round of coding, she might classify what the participants discuss with a finer tag, like "conversation_package" if they were talking about creating packages in R.  In another round, she might get even more specific with codes such as "conversation_package_nomoney" if a participant discussed not having money to create a package in R. Later rounds involve conflating codes that might mean the same thing, relating codes to one another (often by documenting their meanings in a similar way to software documentation), and eliminating codes that no longer make sense.

Sometimes the QDA process involves looking for instances that demonstrate some concept, mechanism, or theory from the academic literature on the subject. This approach is common toward the end of the process in fields such as organization studies, management, and other disciplines and is prominent as an approach from the beginning in fields with experimental or clinical roots (e.g., psychology).

Using software for QDA allows researchers to nest codes, then begin to see the number of instances in which a particular code has been applied. Perhaps more importantly, software allows easy visualization and analysis of where codes co-occur (i.e., where multiple codes have been applied to the same snippets of text), and other linking activities that help researchers identify and specify themes in the data. Some software packages allow the researcher to visualize codes and the relationships between them in new and innovative ways; however, a cursory review of the literature suggests that qualitative researchers often use very basic features; some even use software such as Endnote on Microsoft Word or even analog systems such as paper or sticky notes.

QDA is an iterative process. Researchers will often change, lump together, split, or re-organize codes as they analyze their data. Depending on the coding approach, researchers might also create a codebook or research notes for each code that defines the code and specifies instances in which the code should be applied. 

## Current QDA software

To date, researchers who conduct QDA largely rely upon proprietary software such as those listed below (credit to [bduckles](https://github.com/bduckles) for the descriptions): 

* [NVivo – QSR International](http://www.qsrinternational.com/product)
Desktop based software with both Windows and Mac support. Has a lot of functionality including video and images as data and auto-coding processes. Tech support and training is available and a relatively large user base. 

* [Atlas.TI](http://atlasti.com/) Desktop based software for Mac and Windows, also has mobile apps for android and iOS. Similar functionality as NVivo including using video and visuals as data. Has training and support. 

* [MAXQDA](http://www.maxqda.com/) Desktop based software that works for both Mac and Windows. Training and Several tiers of licenses.

* [QDA Miner – Provalis Research](https://provalisresearch.com/products/qualitative-data-analysis-software/) A Windows application to analyze qualitative text, can be used with some visuals as well. They also have a “lite” package which is free. 

* [HyperResearch – Researchware](http://www.researchware.com/products/hyperresearch.html) Desktop based software that works on both Mac and Windows. 

* [Dedoose](http://www.dedoose.com/) Web based software, usable on any platform. Data stored in the web instead of on your device. Includes text, photos, audio, video and spreadsheet information. 

* [Annotations](http://www.annotationsapp.com/) Mac only app that allows you to highlight, keyword and create notes for text documents. An inexpensive way to do basic qualitative data analysis. Last updated in 2014. 

* [RQDA](http://rqda.r-forge.r-project.org/) A QDA package for R that is free and open source. Bugs in the program make it challenging to use. Last updated in 2012. 

* [Coding Analysis Toolkit – CAT](http://cat.texifter.com/) A free, web based, open source service that was created by University of Pittsburgh. Can be used on any platform. Not supported. 

* [Weft QDA](http://www.pressure.to/qda/) A free and open source Windows program that has no support. The website says that bugs in the program can result in the loss of data. 

## Limitations of Existing Software

Each extant software package has its limitations. The foremost limitation is cost, which can prohibit students and underfunded qualitative researchers from conducting analyses systematically, and efficiently. Furthermore, the mature software packages (e.g., Atlas.TI, NVIVO) offer features that exceed the needs of many users and, as a result, suffer speed issues (particularly for those researchers who may not benefit from advanced hardware). The sharing process for proprietary QDA outputs is equally unwieldy, relying on non-intuitive bundling and unbundling processes, steep learning curves, and non-transferable skill development. 

Open source languages such as R offer the opportunity to involve qualitative researchers in open source software development. Greater involvement of qualitative researchers serves to expand the scope of R users and could create inroads to connect qualitative and quantitative R packages. For instance, better integration of qualitative research packages into R would make it possible for existing text analysis programs to work alongside qualitative coding.  

## User Needs

We began this project at rOpenSci’s [runconf18](https://github.com/ropensci/unconf18) based on [bduckles](https://github.com/bduckles) issue. We began by discussing the common challenges we and our peers face in conducting QDA and considering what we might learn from existing quantitative and text analysis open source packages. We identified a number of unique challenges qualitative researchers encounter when analyzing data. 

* *GUI for broader adoption.* Quantitative researchers are often familiar with conducting analyses from the command line. Qualitative researchers, on the other hand, often rely upon Graphical User Interfaces (GUI) to perform analysis tasks. A proposed, in-development solution is integrating Shiny apps to permit a personalized GUI for QCoder. 

* *Collaboration.* Several aspects of collaboration in qualitative research present challenges for development and use of QDA software. The first challenge relates to version control and tracking changes. Whereas contributors to the open source software community, and perhaps the software development community more generally, have developed processes for sharing code, data, and other work products, qualitative researchers often rely on email, DropBox/box, and other sharing technologies that lack appropriate version control features. One proposed solution is to implement a system of record-keeping that maintains all files akin to Atlas.TI’s bundling and unbundling process, but that permits simultaneous work and comparison/merging of changes. We might also consider urging users to learn git and facilitate skill development by offering tutorials. 
  The second collaboration challenge relates to the need to establish inter-rater or inter-coder reliability. Existing methods of assessing inter-rater reliability for QDA include Cronbach’s Alpha and Krippendorff's Alpha, among others. Such evaluations, though, do not account for complexities such as decisions not to code a segment of text. In other words, by deciding not to apply a code to a snippet of text might be considered an equivalent level of agreement as a decision to apply the same code to the same snippet of text. QCoder aims to make inter-rater reliability assessments easier and more effective than traditional approaches by allowing direct comparisons of differences in the codebooks it generates. 

* *Privacy concerns.* Qualitative data often includes personally-identifying information. Quantitative approaches to deidentification are not always applicable to qualitative datasets because text-based data includes rich descriptions that may elevate the risk of re-identification. Furthermore, qualitative data tends to be unstructured in ways that make systematic approaches to de-identification difficult to implement. QDA software, then, needs to account for these complexities.

* *Reproducibility.* Qualitative research communities, particularly those communities employing methods to probe human and social behavior (such as ethnography), fiercely debate whether or not qualitative research is meant to be reproducible. In other words, some researchers argue that human and social behavior is contextually bound by moments in time, physical settings, and the generally fluid nature of the phenomena under study. Accepting this dispute as valid, we are striving to develop a package that can facilitate some level of reproduction of data analysis. 
  Our approach centers on the production of codebooks. Qualitative researchers create codebooks while coding their data. These codebooks match the code with a description of how and when the code is used. The creation of the codebook is an iterative process that is concurrent with the coding process. Codes are routinely combined, split and shifted as the researcher does their analysis. Codebooks are a way for the researcher to indicate to other members of their research team how to apply codes to the data. Historically, these code books have not been standard across different QDA packages. 
  There could be a benefit to having a standard codebook format which could be compared across projects and even as a project iterates. Using Git or other version control as a codebook is created could prevent mistakes and be a teaching and learning tool for qualitative research. Additionally, most codebooks do not contain sensitive or private data and they could be shared and made publicly available. One could envision the capacity for researchers to share their codebooks so that research that follows could draw on existing codes for future research. A key characteristic of QCoder is that it exposes QDA processes to critique in ways that proprietary software excludes. For example, it is unreasonable to expect that researchers interested in reviewing or reproducing the analysis done for a manuscript pay for a license to perform the task at hand. QCoder can be easily installed and potentially maintained so that critical mistakes in analysis might be caught and corrected in review processes.

* *Money.* Some of the disciplines in which qualitative work occurs do not enjoy the same level of funding as, say, science and engineering. Expensive software packages, then, may not be available to researchers. The case is equally dire for students and researchers in under-resourced communities and countries. An R-based QDA package enables broader participation in qualitative research and, by extension, broader perspectives on the nature of human and social behavior.  

## Conceptualizing a Minimum Viable Product: A vignette

Questa the graduate student is working on one chapter of her dissertation research where she is trying to understand how different codes of conduct in the software community welcome underrepresented communities. She wants to understand language use and to trace how these codes of conduct are created from differing communities. She also is planning to do interviews with people who have created and used these codes of conduct to understand how their adoption influences inclusion.  
 
None of the grants that Questa sent out have come back with any money to do this research. She’s discouraged but passionate about her work and she knows she needs to finish this chapter of her dissertation so she can graduate already. She’s done some work with a nonprofit and they support her work. She’s pretty sure she has a postdoc with them when she finishes her degree. While they should be able to hire her and do support her research, it’s unlikely they’d have enough funds for her to get a license for one of the QDA packages. She has been able to try out some of the larger QDA packages and they work ok with her student license, but her computer is on its last legs and the program keeps crashing her computer. 

She’s planning to start up her interviews and overall she thinks she’ll have maybe around 50-75 text files that include 1) codes of conduct 2) interview transcripts 3) field research at a conference. 

She’s not sure how many codes she’d need, but she’s guessing that the first round of the coding would be a lot of different codes - maybe 150 - 200 codes to start. Then she’d likely change them and boil it down into groups of codes and categories of those codes. So ideally the program would make it easy for her to edit, change and move around the codes. She also needs to write up a simple codebook as she does the coding. 

## Contributors 
* [Elin Waring](https://github.com/elinw) 
* [Dan Sholler](https://github.com/dsholler)
* [Jenny Draper](https://github.com/learithe)
* [Beth Duckles](https://github.com/bduckles)

## References

Corbin, J., & Strauss, A. L. (2014). Basics of qualitative research. Thousand Oaks, CA: Sage.

Glaser, B. G., & Strauss, A. L. (2017). Discovery of grounded theory: Strategies for qualitative research. London, UK: Routledge.

Miles, M. B., & Huberman, A. M. (1994). Qualitative data analysis: An expanded sourcebook. Thousand Oaks, CA: Sage.

Miles, M. B., Huberman, A. M., & Saldana, J. (2013). Qualitative data analysis. Thousand Oaks, CA: Sage.

Saldaña, J. (2015). The coding manual for qualitative researchers. Thousand Oaks, CA: Sage.


