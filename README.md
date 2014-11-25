# NAME

PDL::IO::CSV - Create PDL from CSV file (optimized for speed and large data)

# SYNOPSIS

    my $pdl = rcsv2D($csv_filename_or_filehandle);
    #or
    my $pdl = rcsv2D($csv_filename_or_filehandle, \@column_ids);
    #or
    my $pdl = rcsv2D($csv_filename_or_filehandle, \%options);
    #or
    my $pdl = rcsv2D($csv_filename_or_filehandle, \@column_ids, \%options);

# DESCRIPTION

The traditionall way of creating PDL piddle from CSV data is via [rcols](https://metacpan.org/pod/PDL::IO::Misc#rcols) function.

    my $pdl = rcols("data.csv", [1..4], { DEFTYPE=>double, COLSEP=>"," });

This module provides alternative implementation based [Text::CSV\_XS](https://metacpan.org/pod/Text::CSV_XS) which should be significantly faster than
traditional approach.

# FUNCTIONS

## rcsv1D

Loads data from CSV file into 1D piddles (separate for each column).

    my ($pdl1, $pdl2, $pdl3) = rcsv1D($csv_filename_or_filehandle);
    #or
    my ($pdl1, $pdl2, $pdl3) = rcsv1D($csv_filename_or_filehandle, \@column_ids);
    #or
    my ($pdl1, $pdl2, $pdl3) = rcsv1D($csv_filename_or_filehandle, \%options);
    #or
    my ($pdl1, $pdl2, $pdl3) = rcsv1D($csv_filename_or_filehandle, \@column_ids, \%options);

Items supported in `options` hash:

- type

    Defines the type of output piddles: `double`, `float`, `longlong`, `long`, `short`, `byte`.
    Default value is `double`.

    You can set one type for all columns/piddles:

        my ($a, $b, $c) = rcsv1D($csv, {type => double});

    or separately for each colum/piddle:

        my ($a, $b, $c) = rcsv1D($csv, {type => [long, double, double]});

- fetch\_chunk

    We do not try to load all CSV data into memory at once, we load them in chunks defined by this parameter.
    Default value is `40000` (CSV rows).

- reshape\_inc

    As we do not try to load the whole CSV file into memory at once, we also do not know at the beginning how
    many rows there will be. Therefore we do not know how big piddle to allocate, we have to incrementally
    (re)alocated the piddle by increments defined by this parameter. Default value is `80000`.

    If you know how many rows there will be you can improve performance by setting this parameter to expected row count.

- empty2bad

    Values `0` (default) or `1` - convert empty cells to BAD values (there is a performance cost when turned on).
    If not enabled the BAD values are silently converted into `0`.

- text2bad

    Values `0` (default) or `1` - convert values that don't pass [looks\_like\_number](https://metacpan.org/pod/Scalar::Util#looks_like_number)
    check to BAD values (there is a significant performance cost when turned on). If not enabled these non-numerical
    values are silently converted into `0`.

- header

    Values `0` (default) or `1` - consider the first line as column headers (it is ignored but must have the same count
    of columns as the rest of the CSV file).

- decimal\_comma

    Values `0` (default) or `1` - accept `,` (comma) as a decimal separator (there is a performance cost when turned on).

- encoding

    Optional enconding e.g. `:utf8` (default `undef`) that will be applied on input filehandle.

- debug

    Values `0` (default) or `1` - turn on/off debug messages

- sep\_char

    Value separator, default value `,` (comma).

- and all other options valid for [new](https://metacpan.org/pod/Text::CSV_XS#new) method of [Text::CSV\_XS](https://metacpan.org/pod/Text::CSV_XS)

## rcsv2D

Loads data from CSV file into 2D piddle.

    my $pdl = rcsv2D($csv_filename_or_filehandle);
    #or
    my $pdl = rcsv2D($csv_filename_or_filehandle, \@column_ids);
    #or
    my $pdl = rcsv2D($csv_filename_or_filehandle, \%options);
    #or
    my $pdl = rcsv2D($csv_filename_or_filehandle, \@column_ids, \%options);

Items supported in `options` hash are the same as by ["rcsv1D"](#rcsv1d).

## wcsv1D

Saves data from one or more 1D piddles to CSV file.

    wcsv1D($pdl1, $pdl2, $pdl3, $csv_filename_or_filehandle, \%options);
    #or
    wcsv1D($pdl1, $pdl2, $pdl3, $csv_filename_or_filehandle);
    #or
    wcsv1D($pdl1, $pdl2, \%options); #prints to STDOUT
    #or
    wcsv1D($pdl1, $pdl2);

    # but also
    $pdl1D->wcsv1D("file.csv");

Items supported in `options` hash:

- header

    Arrayref with values that will be printed as the first CSV line.

- bad2empty

    Values `0` or `1` (default) - convert BAD values into empty strings (there is a performance cost when turned on).

- encoding

    Optional enconding e.g. `:utf8` (default `undef`) that will be applied on output filehandle.

- debug

    Values `0` (default) or `1` - turn on/off debug messages

- sep\_char

    Value separator, default value `,` (comma).

- eol

    New line separator, default value `\n` (UNIX newline).

- and all other options valid for [new](https://metacpan.org/pod/Text::CSV_XS#new) method of [Text::CSV\_XS](https://metacpan.org/pod/Text::CSV_XS)

## wcsv2D

Saves data from one 2D piddle to CSV file.

    wcsv2D($pdl, $csv_filename_or_filehandle, \%options);
    #or
    wcsv2D($pdl, $csv_filename_or_filehandle);
    #or
    wcsv2D($pdl, \%options); #prints to STDOUT
    #or
    wcsv2D($pdl);

    # but also
    $pdl->wcsv2D("file.csv");

Items supported in `options` hash are the same as by ["wcsv1D"](#wcsv1d).

# SEE ALSO

[PDL](https://metacpan.org/pod/PDL), [Text::CSV\_XS](https://metacpan.org/pod/Text::CSV_XS)

# LICENSE

This program is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

# COPYRIGHT

2014+ KMX <kmx@cpan.org>
