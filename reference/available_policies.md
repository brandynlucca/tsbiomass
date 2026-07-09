# List available policy components

Returns the policy groups, aggregation metrics, slope branches, and
materialized policy definitions available from the policy registry.

## Usage

``` r
available_policies(policy_path = NULL)
```

## Arguments

- policy_path:

  Optional path to a policy registry JSON file.

## Value

A named list with `groups`, `metrics`, `branches`, and `policies`
tibbles.

## Examples

``` r
available_policies()
#> $groups
#> # A tibble: 4 × 6
#>   key         coded_suffix    candidate_pool display_phrase display_phrase_title
#>   <chr>       <chr>           <chr>          <chr>          <chr>               
#> 1 study_cell  within_study_c… closest_study… within-study-… Within-study-cell   
#> 2 all         across_all_adm… all_admissible across all ad… Across all admissib…
#> 3 generalized generalized     generalized_m… generalized    Generalized         
#> 4 cluster     within_cluster  same_nmds_clu… within-cluster Within-cluster      
#> # ℹ 1 more variable: candidate_pool_definition <chr>
#> 
#> $metrics
#> # A tibble: 7 × 7
#>   key             coded_prefix policy_family aggregation_method display_template
#>   <chr>           <chr>        <chr>         <chr>              <chr>           
#> 1 closest         closest_     single_model  nearest_by_combin… Closest {group_…
#> 2 weighted_mean   weighted_me… weighted_ens… kernel_weighted_m… {group_phrase_t…
#> 3 unweighted_mean unweighted_… unweighted_e… arithmetic_mean    {group_phrase_t…
#> 4 survey_distance survey_dist… single_model  nearest_by_trait_… Survey-distance…
#> 5 taxon_distance  taxon_dista… single_model  nearest_by_taxono… Taxon-distance …
#> 6 species_distan… species_dis… single_model  nearest_by_specie… Species-distanc…
#> 7 random          random_      random_basel… random_draw        Random {group_p…
#> # ℹ 2 more variables: aggregation_definition <chr>, n_random_draws <list>
#> 
#> $branches
#> # A tibble: 3 × 6
#>   key             display_name display_tag description  aliases row_filter_value
#>   <chr>           <chr>        <chr>       <chr>        <list>  <list>          
#> 1 all             All slopes   all-slopes  Allow both … <list>  <NULL>          
#> 2 fixed20_only    Fixed slope  fixed-slope Restrict th… <list>  <chr [1]>       
#> 3 free_slope_only Free slope   free-slope  Restrict th… <list>  <chr [1]>       
#> 
#> $policies
#> # A tibble: 181 × 13
#>    coded_name              display_name description policy_family candidate_pool
#>    <chr>                   <chr>        <chr>       <chr>         <chr>         
#>  1 closest_within_class    Closest wit… Closest wi… single_model  match_all_tra…
#>  2 weighted_mean_within_c… Within Clas… Within Cla… weighted_ens… match_all_tra…
#>  3 unweighted_mean_within… Within Clas… Within Cla… unweighted_e… match_all_tra…
#>  4 closest_within_order    Closest wit… Closest wi… single_model  same_order    
#>  5 weighted_mean_within_o… Within Orde… Within Ord… weighted_ens… same_order    
#>  6 unweighted_mean_within… Within Orde… Within Ord… unweighted_e… same_order    
#>  7 closest_within_family   Closest wit… Closest wi… single_model  same_family   
#>  8 weighted_mean_within_f… Within Fami… Within Fam… weighted_ens… same_family   
#>  9 unweighted_mean_within… Within Fami… Within Fam… unweighted_e… same_family   
#> 10 closest_within_genus    Closest wit… Closest wi… single_model  same_genus    
#> # ℹ 171 more rows
#> # ℹ 8 more variables: aggregation_method <chr>, fixed_parameters <list>,
#> #   tunable_parameters <list>, grouping_key <chr>, metric_key <chr>,
#> #   candidate_pool_definition <chr>, aggregation_definition <chr>,
#> #   plain_language_definition <chr>
#> 
```
