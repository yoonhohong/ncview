# NCView   

## Goal 

NCView is an interactive web application which offers a basic yet useful tool that simplifies the exploration and analysis of NCS data. NCView is freely available, open source, and accessible via the Internet and can be used within a web browser. It is developed using R and Shiny package, and deployed in shinyapps.io. The application can be used to summarize, visualize and analyze data.   

## Motivation 

Currently, most EMG laboratories have presented the results of NCS, with raw values mostly in a table format and/or descriptions of the findings. There are several problems in this reporting practice. First of all, raw values may be difficult to interpret without information about the normal cutoff values which may vary among EMG labs. Secondly, table may be difficult to read for readers unfamiliar with the structure of data reporting table which is not standardized across different EMG systems. Lastly but not leastly, since the current reporting system provides the result in a snap-shot manner, it is difficult to analyze the changes with disease progression or treatment. There may be better ways to communicate the results of NCS.

Data visualization is a graphical representation of data using visual elements. It can make analysis much easier and faster, allowing us to recognize the patterns in data which might go unnoticed with just a table of raw numbers. With interactive features to manipulate the parameters of a data visualization, users can dynamically explore the data more efficiently, which can make data visualization an even more effective way to capture the full value of the data.

## Methods 

NCView provides three modes to visualize the data.

- The first mode is "tile plot", which provides a summary table with color-coded percentage values relative to normal cutoffs (first tab) and pathological cutoffs for CIDP (fourth tab, 2021 criteria).
- The second mode is "radial plot", which consists of `parameter view` and `nerve view`. In the parameter view, each axis represents different conduction parameter (eg, CMAP amplitudes, distal motor latency, dCMAP duration, conduction velocity, etc), and each line represents different nerve (eg, median, ulnar, etc.). This view can help recognize the `pathological pattern of conduction abnormalities` in each nerve, and compare the pattern across different nerves. Meanwhile, in the nerve view, each axis represents different nerve, while each line represents different conduction parameter. This view can help recognize the `spatial pattern of conduction abnormalities` in each parameter, and compare the pattern across different parameters.
- The third mode is "spaghetti plot", which is implemented in FU (follow-up) tab and visualizes the longitudinal changes of conduction parameters (percentage values relative normal cutoffs) in each nerves.

Input data should take a wide format    
- Meta-data: Hosp, Date, Name, ID, Sex, Age   
- 112 NCS parameters (values should be expressed as percentage values relative upper or lower limit of normal as appropriate)    


radial plot 
- parameter view (across different nerves)
- nerve view (across different parameters) 
	
spaghetti plot 


## Results 
## Discussion 
## References 

NCView: A Web Application for Summary and Interactive Analysis of Nerve Conduction Study Data (in preparation) 
This paper describes the application and gives detailed examples describing how to use the application on real data from a clinical study including patients with CIDP.



	
