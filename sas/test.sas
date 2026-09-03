/* Generate a manageable synthetic dataset for mixed effects modeling */
data large_study_data;
    /* Set random seed for reproducibility */
    call streaminit(12345);
    
    /* Generate ~400 observations - more manageable for memory */
    do study_id = 1 to 5;        
        do site_id = 1 to 2;     
            do subject_id = 1 to 20; /* Reduced from 50 to 20 */
                
                /* Create unique subject identifier across all studies/sites */
                unique_subject_id = (study_id * 100) + (site_id * 20) + subject_id;
                
                /* Generate subject-level random effects */
                subject_intercept = rand('normal', 0, 2);
                
                /* Multiple measurements per subject (longitudinal data) */
                do visit = 1 to 4;
                    
                    /* Time-varying covariates */
                    age = 18 + rand('uniform') * (75 - 18);
                    gender = rand('bernoulli', 0.52); /* 52% female */
                    treatment = rand('bernoulli', 0.5); /* 50% treatment */
                    baseline_score = rand('normal', 50, 10);
                    
                    /* Study and site effects */
                    study_effect = rand('normal', 0, 1.5);
                    site_effect = rand('normal', 0, 1.0);
                    
                    /* Generate outcome with complex mixed effects structure */
                    time_trend = visit * 2.5;
                    treatment_effect = treatment * 8.3;
                    age_effect = (age - 45) * 0.2;
                    gender_effect = gender * 3.1;
                    
                    /* Random error */
                    error = rand('normal', 0, 3.2);
                    
                    /* Final outcome */
                    outcome = 45 + 
                             time_trend + 
                             treatment_effect + 
                             age_effect + 
                             gender_effect + 
                             study_effect + 
                             site_effect + 
                             subject_intercept + 
                             error;
                    
                    /* Add some missing data patterns */
                    if rand('uniform') < 0.05 then outcome = .;
                    if rand('uniform') < 0.02 then age = .;
                    
                    /* Create categorical versions */
                    select;
                        when (age < 30) age_group = 'Young';
                        when (age < 50) age_group = 'Middle';  
                        when (age >= 50) age_group = 'Older';
                        otherwise age_group = 'Unknown';
                    end;
                    
                    select;
                        when (treatment = 1) treatment_group = 'Active';
                        otherwise treatment_group = 'Placebo';
                    end;
                    
                    select;
                        when (gender = 1) gender_label = 'Female';
                        otherwise gender_label = 'Male';
                    end;
                    
                    output;
                end;
            end;
        end;
    end;
run;

/* Check the dataset size */
proc sql;
    select count(*) as total_observations,
           count(distinct study_id) as num_studies,
           count(distinct site_id) as num_sites,
           count(distinct unique_subject_id) as num_subjects
    from large_study_data;
quit;

/* Simple PROC MIXED analysis with residuals output */
proc mixed data=large_study_data method=reml;
    class unique_subject_id study_id treatment_group visit;
    model outcome = visit treatment_group visit*treatment_group /
          solution outp=mixed_results;  /* This outputs residuals */
    random intercept / subject=unique_subject_id;
    random intercept / subject=study_id;
    repeated visit / subject=unique_subject_id type=ar(1);    
    ods output SolutionF=fixed_effects
               CovParms=variance_components;
run;

/* Check residuals and model diagnostics */
proc univariate data=mixed_results plots;
    var resid;
    histogram;
    qqplot;
    title 'Residual Analysis for Mixed Model';
run;

/* Create diagnostic plots */
proc sgplot data=mixed_results;
    scatter x=pred y=resid;
    refline 0 / axis=y;
    title 'Residuals vs Predicted Values';
run;