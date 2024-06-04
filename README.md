<!-- Improved compatibility of back to top link: See: https://github.com/othneildrew/Best-README-Template/pull/73 -->
<a name="readme-top"></a>
<!--
*** Thanks for checking out the Best-README-Template. If you have a suggestion
*** that would make this better, please fork the repo and create a pull request
*** or simply open an issue with the tag "enhancement".
*** Don't forget to give the project a star!
*** Thanks again! Now go create something AMAZING! :D
-->



<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/WWLDS/predictingDNA">
  </a>

  <h3 align="center">WWL DNA Predictor</h3>

  <p align="center">
    A ML algorithm that highlights patients that are likely to not attend their appointment!
    <br />
    <a href="https://github.com/WWLDS/predictingDNA"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/WWLDS/predictingDNA/issues/new?assignees=&labels=bug&projects=&template=bugReport.md&title=">Report Bug</a>
    ·
    <a href="https://github.com/WWLDS/predictingDNA/issues/new?assignees=&labels=enhancement&projects=&template=featureRequest.md&title=">Request Feature</a>
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
      </ul>
    </li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

According to NHS-England, during 2021/22 almost 7.5 million Outpatient appointments were missed by patients, equating to a significant cost both financially and to patient health outcomes. Evidence suggests that although the reasons for missed appointments are varied, those patients from the most deprived groups are most at risk.

A team led by the Department of Data Analytics and Assurance of Wrightington, Wigan and Leigh NHS Trust set out to develop a machine learning model to understand and predict those patients most likely to not attend an appointment. Moreover, we designed a prospective four-armed case-control experiment to test the efficacy of a second text reminder intervention on a subset of our outpatients. The four-arms consisted of a high-probability to DNA (Did Not Attend) group and a low-probability to DNA group patients, which were randomly split 50:50 into intervention and control. Intervention groups received a text reminder 5 and 3 days before their appointment, while the control groups received a single text reminder 5 days before their appointment.

Our trial ran for a period of three months (January – April 2024; n = 28000 outpatient appointments) and showed a statistically significant reduction in the DNA rate of high probability and low probability patients receiving the intervention (absolute difference of 1.8% and 1.2%, respectively). This work has recently received our Trusts support to expand the set of patients receiving the intervention to all Outpatients and the work is currently being written up as an academic manuscript for sharing.

The repository includes a minimal and pseudonymised script and dataset (upon request) that enables other users to reproduce our work. It should be noted that the script contained in this repository contains none of the data cleaning steps, a small fraction of the data engineering and several missing features. Despite these limitations, we believe that an experienced NHS Hospitals' Data Scientist should be able to fully reproduce the features used in our model by reverse engineering. We have left in the more complicated and most necessary data engineering, including building nth order Markov Chain's and ensuring sample independence. For a full explanation of any of the steps taken within the code, please do reach out.


<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- GETTING STARTED -->
## Getting Started

- Contact creator (email linked above) for the pseudonymised dataset required to run the associated code.
- Clone the repository to your local drive.
- Save the pseudonymised dataset to your local PredDNA repository folder (place within "/rawData/").
- Open and run the script. No changes to the code are necessary for correct running of the script.

### Prerequisites

- R Studio

<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Thomas Ingram -  thomas.ingram@wwl.nhs.uk

Project Link: [https://github.com/WWLDS/PredDNA](https://github.com/WWLDS/predictingDNA)

<p align="right">(<a href="#readme-top">back to top</a>)</p>


