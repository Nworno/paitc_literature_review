# paitc_literature_review
Literature review of population adjusted indirect treatment comparison

File `dm_extractions.R`: 

	Data management, to process extraction results, and compare answers between reviewers.

File `parse_pubmed_text_export.R`: 

	Transform a Pubmed abstract or PubMed format export into csv file, and to be imported in Rayyan.ai
	NB: rayyan.ai can also use directly the pubmed format to import data, but export is in csv format so to be able to merge the two eventually it's still useful to transform pubmed data to csv data. 
	
`rayyan_output_dm.R`: 

  Transform rayyan output to get included articles and exclusion reasons

Data folder: 
  - articles_selection: 
    - pubv1_wide_format.csv: original export from PubMed query
    - pubmed-indirecttr-set_abstract.txt: export from PubMed with more metadata details about articles
    - pubv1_formatted_rayyan_input.csv: formatted to be useable Rayyan.ai ()
    - included_articles.csv: final list of articles included in the extraction
  - extraction:
    - **comparison_answers2022-09-21.csv**: extraction data-managed in a long format --> can be used for the article statistics. Column "decision".
    - current_extraction.csv: extraction in a wide format
    - results_summary.csv: stats about article answers
