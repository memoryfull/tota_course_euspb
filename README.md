# Text-as-data analysis of international trade
###### Dmitriy Skougarevskiy, European University at St. Petersburg
<img align="center" src="http://mappinginvestmenttreaties.com/misc/tota_course_2018/course_image_small.jpg">

# Overview

With multilateral negotiations at the World Trade Organization (WTO) in deadlock, rule-making on international economic governance has shifted to preferential trade agreements (PTAs). This short 5-week course seeks to introduce the audience to recent trends in international economic order in quantitative fashion. We will discuss the workhorse model of empirical international trade in goods and extend it with the aid of the textual information on trade law covering those flows.

In doing so we will learn how to gather information on merchandise trade, predict trade flows, and represent texts as vectors. Practical sessions will take the theory to statistical packages; the course is hands-on, with each lecture followed by a code review session.

# Course Requirements

**Readings** The course consists of lectures and lab sessions. Each lecture and lab session is accompanied by required readings. I expect the participants to have read the required materials by the beginning of each lecture or session.

**Code** During lab sessions we will review and execute pre-written code together. Code snippets will be disseminated before each session in `code/` folder in this repository. Participants are required to have the code ready on their laptops before each lab session.
**Technical requirements** Each lab session will be conducted hands-on, with your laptops. I expect you to come to the first lab session with base R (https://www.r-project.org) preinstalled.
**Evaluation** Since this is a zero-credit course, there will be no evaluation.

**Working language** English.

# Course Schedule

#### Lecture 1 | Preferential Trade Agreements and International Economic Order ([`slides`](https://github.com/memoryfull/tota_course_euspb/blob/master/lectures/lecture_1_Skougarevskiy_text_as_data_international_trade.pdf))*
-   Chauffour, J.-P. and J.-C. Maur (eds). 2011. *Preferential Trade
    Agreement Policies for Development: A Handbook*. Chapter 2
    **(required)**. Chapter 6 (optional). <http://hdl.handle.net/10986/2329>
-   Alschner, W. and J. Seiermann and D. Skougarevskiy. 2017.
    Text-as-data analysis of preferential trade agreements: Mapping the
    PTA landscape.
    <http://unctad.org/en/pages/PublicationWebflyer.aspx?publicationid=1838>
    **(required)**

#### Lab Session 1 | Working with XMLs in R ([`code`](https://github.com/memoryfull/tota_course_euspb/blob/master/code/1_lecture_reading_tota_xmls.r))

-   `ToTA`: Texts of Trade Agreements repository.
    <https://github.com/mappingtreaties/tota/> **(required)**
-   `xml2` tutorial. <https://blog.rstudio.com/2015/04/21/xml2/>
    **(required)**
-   XPath cheatsheet.
    <https://data-lessons.github.io/library-webscraping/extras/xpath-cheatsheet.md.pdf>
    (optional)

#### Lecture 2 | Gravity and Gravitas ([`slides`](https://github.com/memoryfull/tota_course_euspb/blob/master/lectures/lecture_2_Skougarevskiy_text_as_data_international_trade.pdf))

-   Shepherd, B. 2016. *The Gravity Model of International Trade: A User
    Guide*. UNESCAP.
    <http://www.unescap.org/sites/default/files/GravityUserGuide-REVISED-02.pdf>
    **(required)**
-   Baldwin R. and D. Taglioni. 2006. Gravity for Dummies and Dummies
    for Gravity Equations. NBER Working Paper No. 12516.
    <http://www.nber.org/papers/w12516> (optional)

#### Lab Session 2 | Working with `COMTRADE` ([`code`](https://github.com/memoryfull/tota_course_euspb/blob/master/code/2_working_with_comtrade.r))

-   What is UN Comtrade?
    <https://unstats.un.org/unsd/tradekb/Knowledgebase/50075/What-is-UN-Comtrade>
    **(required)**
-   Using the UN Comtrade data API with R.
    <https://comtrade.un.org/data/Doc/api/ex/r> **(required)**
-   United Nations. *International Trade Statistics Yearbook*. 2016.
    Volume I. <https://comtrade.un.org/ITSY2016VolI.pdf> (skim through
    pp. viiix)

#### Lecture 3 | Text factorisation I: Bag-of-words methods ([`slides`](https://github.com/memoryfull/tota_course_euspb/blob/master/lectures/lecture_3_Skougarevskiy_text_as_data_international_trade.pdf))

-   Gentzkow, M. and B. Kelly and M. Taddy. 2017. Text as data. NBER
    Working Paper No. 23276 <http://www.nber.org/papers/w23276>
    (required, pp. 111)
-   Jurafsky, D. and J. Martin. 2016... *Speech and Language
    Processing*. Chapter 15.1. **(required)**. Chapter 4.1 (optional)
    <https://web.stanford.edu/~jurafsky/slp3/15.pdf>,
    <https://web.stanford.edu/~jurafsky/slp3/4.pdf>

#### Lab session 3 | Reducing PTA document-term matrices to latent spaces ([`code`](https://github.com/memoryfull/tota_course_euspb/blob/master/code/3_bow_representations.r))

-   Selivanov, D. Vectorization <http://text2vec.org/vectorization.html>
    **(required)**

#### Lecture 4 | Text factorisation II: Distributional semantics ([`slides`](https://github.com/memoryfull/tota_course_euspb/blob/master/lectures/lecture_4_Skougarevskiy_text_as_data_international_trade.pdf))

-   Jurafsky, D. and J. Martin. 2016... *Speech and Language
    Processing*. Chapter 15.4. **(required)**. Chapter 16.2. (optional)
    <https://web.stanford.edu/~jurafsky/slp3/15.pdf>,
    <https://web.stanford.edu/~jurafsky/slp3/16.pdf>

#### Lab session 4 | Learning the meaning of legal terms in PTAs with embeddings ([`code`](https://github.com/memoryfull/tota_course_euspb/blob/master/code/4_distributional_semantics.r))

-   Selivanov, D. GloVe Word Embeddings <http://text2vec.org/glove.html>
    **(required)**
-   `StarSpace`. <https://github.com/facebookresearch/starSpace>
    **(required)**
-   Wu, L. et al. 2018. StarSpace: Embed All The Things!
    <https://arxiv.org/abs/1709.03856> (optional)

#### Lecture 5 | Welfare effects of Preferential Trade Agreements ([`slides`](https://github.com/memoryfull/tota_course_euspb/blob/master/lectures/lecture_5_Skougarevskiy_text_as_data_international_trade.pdf))

-   Chauffour, J.-P. and J.-C. Maur (eds). 2011. *Preferential Trade
    Agreement Policies for Development: A Handbook*. Chapter 3
    **(required)**. <http://hdl.handle.net/10986/2329>
-   Baier, S. and J. Bergstrand. 2007. Do free trade agreements actually
    increase members’ international trade? *Journal of International
    Economics*. 71, 7295. **(required)**
    <https://doi.org/10.1016/j.jinteco.2006.02.005>

#### Lab Session 5 | Putting the pieces together: PTA embeddings and merchandise trade ([`code`](https://github.com/memoryfull/tota_course_euspb/blob/master/code/5_ptas_and_welfare.r))

-   This session will capitalise on all the previous readings

# License

This course is distributed under the Creative Commons Attribution-NonCommercial 4.0 International license.