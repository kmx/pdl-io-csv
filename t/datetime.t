use strict;
use warnings;

use Test::More;
use PDL;
use PDL::IO::CSV ':all';
use Test::Number::Delta relative => 0.00001;
use Config;

use constant NODATETIME => eval { require PDL::DateTime; require Time::Moment; 1 } ? 0 : 1;

ok(-f 't/_sample4.csv');

if (NODATETIME) {
  diag "SKIP - PDL::DateTime not installed";
}
else {
  # http://ichart.finance.yahoo.com/table.csv?a=8&b=11&e=10&g=d&c=2009&d=2&f=2010&s=YHOO
  my ($Date, $Open, $High, $Low, $Close, $Volume, $AdjClose) = rcsv1D('t/_sample4.csv', { header=>1, detect_datetime=>1});
  is(ref $Date, 'PDL::DateTime');
  is($Date->info, 'PDL::DateTime: LongLong D [124]');
  is($Date->hdr->{col_name}, 'Date');
  is($Date->min,   1252627200000000);
  is($Date->max,   1268179200000000);
  is($Date->sum, 156283344000000000);
}

done_testing;