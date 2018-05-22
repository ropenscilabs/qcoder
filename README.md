# qcoder

Lightweight package to conduct qualitative coding

## Motivation

The motivation for QcodeR stems from the need for a lightweight, free, open source option for analyzing textual qualitative data. Qualitative data refers to text from interview transcripts, observation notes, memos, and primary source/archival documents. 

In this package we enable the user to upload text files, highlight sections of the text, add codes to text quotes and create a description for the codes. The intended output is a dataset which sorts text quotes by code and a codebook linking codes with their description.

Qualitative data analysis (QDA) processes, particularly those developed by Corbin and Strauss (2014), Miles, Huberman, and Saldana (2013), and Glaser and Strauss (2017), can be thought of as layering interpretation onto the text. The researcher starts with open coding, meaning that she is free to tag snippets of text with whatever descriptions she deems appropriate. For instance, if the researcher is coding observation notes and senses that conversations between two individuals will be relevant to the research questions, she might tag the instances in which the two individuals speak with the code "conversation." In the next round of coding, she might classify what the participants discuss with a finer tag, like "conversation_package" if they were talking about creating packages in R.  In another round, she might get even more specific with codes such as "conversation_package_nomoney" if a participant discussed not having money to create a package in R. Later rounds involve conflating codes that might mean the same thing, relating codes to one another (often by documenting their meanings in a similar way to software documentation), and eliminating codes that no longer make sense.

Sometimes the QDA process involves looking for instances that demonstrate some concept, mechanism, or theory from the academic literature on the subject. This approach is common toward the end of the process in fields such as organization studies, management, and other disciplines and is prominent as an approach from the beginning in fields with experimental or clinical roots (e.g., psychology).

Using software for QDA allows researchers to nest codes, then begin to see the number of instances in which a particular code has been applied. Perhaps more importantly, software allows easy visualization and analysis of where codes co-occur (i.e., where multiple codes have been applied to the same snippets of text), and other linking activities that help researchers identify and specify themes in the data. Some software packages allow the researcher to visualize codes and the relationships between them in new and innovative ways; however, a cursory review of the literature suggests that qualitative researchers often use very basic features; some even use software such as Endnote on Microsoft Word.

QDA is an iterative process. Researchers will often change, lump together, split, or re-organize codes as they analyze their data. Depending on the coding approach, researchers might also create a codebook or research notes for each code that defines the code and specifies instances in which the code should be applied. 

## Current QDA software

To date, researchers who conduct QDA largely rely upon proprietary software such as those listed below (credit to Beth Duckles for the descriptions): 

* NVivo – QSR International  http://www.qsrinternational.com/product
Desktop based software with both Windows and Mac support. Has a lot of functionality including video and images as data and auto-coding processes. Tech support and training is available and a relatively large user base. 

* Atlas.TI  http://atlasti.com/
Desktop based software for Mac and Windows, also has mobile apps for android and iOS. Similar functionality as NVivo including using video and visuals as data. Has training and support. 

* MaxQDA http://www.maxqda.com/
Desktop based software that works for both Mac and Windows. Training and Several tiers of licenses.

* QDA Miner – Provalis Research 
https://provalisresearch.com/products/qualitative-data-analysis-software/ 
A Windows application to analyze qualitative text, can be used with some visuals as well. They also have a “lite” package which is free. 

* HyperResearch – Researchware http://www.researchware.com/products/hyperresearch.html 
Desktop based software that works on both Mac and Windows. 

* Dedoose  http://www.dedoose.com/ 
Web based software, usable on any platform. Data stored in the web instead of on your device. Includes text, photos, audio, video and spreadsheet information. 

* Annotations http://www.annotationsapp.com/
Mac only app that allows you to highlight, keyword and create notes for text documents. An inexpensive way to do basic qualitative data analysis. Last updated in 2014. 

* RQDA http://rqda.r-forge.r-project.org/
A QDA package for R that is free and open source. Bugs in the program make it challenging to use. Last updated in 2012. 

* Coding Analysis Toolkit – CAT http://cat.texifter.com/  
A free, web based, open source service that was created by University of Pittsburgh. Can be used on any platform. Not supported. 

* Weft QDA  http://www.pressure.to/qda/ 
A free and open source Windows program that has no support. The website says that bugs in the program can result in the loss of data. 

## Limitations of Existing Software

Each extant software package has its limitations. The foremost limitation is cost, which can prohibit students and underfunded qualitative researchers from conducting analyses systematically and efficiently. Furthermore, the mature software packages (e.g., Atlas.TI) offer features that exceed the needs of many users and, as a result, suffer speed issues (particularly for those researchers who may not benefit from advanced hardware). The sharing process for proprietary QDA outputs is equally unwieldy, relying on non-intuitive bundling and unbundling processes, steep learning curves, and non-transferable skill development. 

Open source languages such as R offer the opportunity to involve qualitative researchers in open source software development. Greater involvement of qualitative researchers serves to expand the scope of R users and could create inroads to connect qualitative and quantitative R packages. For instance, better integration of qualitative research packages into R would make it possible for existing text analysis programs to work alongside qualitative coding.

## Contributors 
(Elin Waring)[https://github.com/elinw] 
(Dan Sholler)[https://github.com/dsholler]
(Jenny Draper)[https://github.com/learithe]
(Beth Duckles)[https://github.com/bduckles]

## References

Corbin, J., & Strauss, A. L. (2014). Basics of qualitative research. Thousand Oaks, CA: Sage.

Glaser, B. G., & Strauss, A. L. (2017). Discovery of grounded theory: Strategies for qualitative research. London, UK: Routledge.

Miles, M. B., & Huberman, A. M. (1994). Qualitative data analysis: An expanded sourcebook. Thousand Oaks, CA: Sage.

Miles, M. B., Huberman, A. M., & Saldana, J. (2013). Qualitative data analysis. Thousand Oaks, CA: Sage.

Saldaña, J. (2015). The coding manual for qualitative researchers. Thousand Oaks, CA: Sage.


