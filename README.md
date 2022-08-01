# paitc_literature_review
Literature review of population adjusted indirect treatment comparison

File `dm_extractions.R`: 
	Data management, to process extraction results, and compare answers between reviewers.

File `parse_pubmed_text_export.R`: 
	Transform a Pubmed abstract or PubMed format export into csv file, and to be imported in Rayyan.ai
	NB: rayyan.ai can also use directly the pubmed format to import data, but export is in csv format so to be able to merge the two eventually it's still useful to transform pubmed data to csv data. 

Data folder: 
	`data/extraction_wide_format.csv`: extraction formatted with all information in wide format
	`data/extraction_formatted_rayyan_input.csv`: additional data management to have a rayyan.ai format ready
