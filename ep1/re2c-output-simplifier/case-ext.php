<?php
$case_begin    = '';
$case_end      = '';
$last_casechar = 0;

$flush_case = function($content) use(&$case_begin,&$case_end)
{
  if(!empty($case_begin))
  {
    print "{$case_begin}{$case_end}:{$content}\n";
    $case_begin = '';
    $case_end   = '';
  }
  elseif(!empty($content))
    print "{$content}\n";
};

foreach(explode("\n", file_get_contents('php://stdin')) as $s)
{
  if(preg_match("@^\tcase '(.)':(.*)@", $s, $mat))
  {
  /*
    Case_begin   Case_end     Last_casechar    Has content       What to do
    empty        empty        -                no                            Capture into case_begin & last_casechar
    nonempty     -            mismatch         no                Flush, then capture into case_begin & last_casechar
    nonempty     -            match            no                Create case_end, update last_casechar
    empty        empty        -                yes               Passthrough
    nonempty     -            mismatch         yes               Flush, then passthrough
    nonempty     -            match            yes               Create case_end, update last_casechar; then flush with content
  */
    $char        = ord($mat[1][0]);
    $has_content = !empty($mat[2]);
    if(strlen($case_begin) == 0) // empty
    {
      if($has_content)
        print "$s\n";
      else
        { $case_begin = "\tcase '".$mat[1]."'"; $last_casechar = $char; }
    }
    else if($char == $last_casechar+1) // match
    {
      $case_end = " ... '".$mat[1]."'"; $last_casechar = $char;
      if($has_content)
        $flush_case($mat[2]);
    }
    else // mismatch
    {
      $flush_case('');
      if($has_content)
        print "$s\n";
      else
        { $case_begin = "\tcase '".$mat[1]."'"; $last_casechar = $char; }
    }
  }
  else
  {
    $flush_case('');
    print "$s\n";
  }
}

$flush_case('');
