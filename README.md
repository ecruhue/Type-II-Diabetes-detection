# Type-II-Diabetes-detection
The goal of this research is to discover early biomarkers for type 2 diabetes, a very common and
debilitating disease. Diagnosis is typically based on high glucose concentrations in the blood while
fasting or after a controlled meal. Unfortunately, the standard usage of these tests has little power to
identify subjects who show signs of moving towards diabetes despite their normal glucose levels.
The following analysis focuses on the relationships between glucose, insulin, and glucagon

Our analysis is based on data from two experiments. One study sampled blood from 41 subjects
every 10 minutes over the course of 9 hours overnight, while the subject fasted. The other is an
experiment in which 76 subjects were given a controlled meal, and had their blood sampled every 5
minutes for 4 hours. In each experiment, due to prior testing, the subjects are known to be diabetic or
non-diabetic according to the traditional fasting and glucose tolerance tests. By analyzing the
relationships between glucose, insulin, and glucagon in these controlled settings, we hope to identify
deeper patterns of interaction that might be early signs of diabetes.

### Two Approaches to the Meal Experiment
We improved our classification in the meal data using two methods. First, we devised additional
features and applied additional algorithms. Second, we combined the groups in a way that simplifies the
problem while capturing the essential part of the question.

### Subspace Discriminant in Nighttime Experiment
As noted above, the challenge in the nighttime fasting data is to use features that do not reflect
the very wide difference between the means and standard deviations of glucose between diabetics and
nondiabetics, and to instead capture some of the self-regulating relationships. To summarize the
variations of secretions across the three substances, we started by taking the difference between
successive timepoints.
