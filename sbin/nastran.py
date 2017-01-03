#!/usr/bin/python
################################################################################
# NASTRAN BOOTSTRAP SCRIPT
# D. Everhart
# 10 FEB 2016
################################################################################
# The MIT License (MIT)
# 
# Copyright (c) 2016 Daniel Everhart
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the 
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# 
#################################################################################  Version 
__version_major__  = 0
__version_minor__  = 0
__version_bugfix__ = 0
__version_info__   = (str(__version_major__),
                      str(__version_minor__),
                      str(__version_bugfix__))
__version__        = '.'.join(__version_info__)
################################################################################
import argparse
import sys
import os.path
import uuid

def main(args):
  sdir = '/tmp'
  workdir = os.path.abspath(os.curdir)
  nastran_home=os.path.abspath(os.path.dirname(sys.argv[0]) + '/..')
  nastran_x = '{0}/bin/nastran.x'.format(nastran_home)

  bname = ''
  if os.path.isfile(args.input):
    bname = os.path.splitext(os.path.split(args.input)[1])[0]
  if len(bname) < 1: raise ValueError('Valid input file not provided.')

  SCRATCH_DIRECTORY  = '{0}/{1}'  .format(sdir,str(uuid.uuid4()).split('-')[0])
  RIGID_FORMAT_DIRECTORY   = '{0}/rf'   .format(nastran_home)

  NPTPNM  = '{0}.nptp' .format(bname)
  PLTNM   = '{0}.plt'  .format(bname)
  DICTNM  = '{0}.dict' .format(bname)
  PUNCHNM = '{0}.pch'  .format(bname)
  OPTPNM  = '{0}.opt'  .format(bname)
  LOGNM   = '{0}.f04'  .format(bname)
  F06     = '{0}.f06'  .format(bname)

  IN12    = '{0}.in12' .format(bname)
  OUT11   = '{0}.out11'.format(bname)

  FTN11   = '{0}.f11'  .format(bname)
  FTN12   = '{0}.f12'  .format(bname)
  FTN13   = '{0}.f13'  .format(bname)
  FTN14   = '{0}.f14'  .format(bname)
  FTN15   = '{0}.f15'  .format(bname)
  FTN16   = '{0}.f16'  .format(bname)
  FTN17   = '{0}.f17'  .format(bname)
  FTN18   = '{0}.f18'  .format(bname)
  FTN19   = '{0}.f19'  .format(bname)
  FTN20   = '{0}.f20'  .format(bname)
  FTN21   = '{0}.f21'  .format(bname)
  FTN22   = '{0}.f22'  .format(bname)
  FTN23   = '{0}.f23'  .format(bname)

  SOF1    = '{0}.sof1' .format(bname)
  SOF2    = '{0}.sof2' .format(bname)
  
  if len(args.output_dir) > 0:
    NPTPNM  = '{0}/{1}'.format(args.output_dir, NPTPNM )
    PLTNM   = '{0}/{1}'.format(args.output_dir, PLTNM  )
    DICTNM  = '{0}/{1}'.format(args.output_dir, DICTNM )
    PUNCHNM = '{0}/{1}'.format(args.output_dir, PUNCHNM)
    OPTPNM  = '{0}/{1}'.format(args.output_dir, OPTPNM )
    LOGNM   = '{0}/{1}'.format(args.output_dir, LOGNM  )
    F06     = '{0}/{1}'.format(args.output_dir, F06    )

    IN12    = '{0}/{1}'.format(args.output_dir, IN12   )
    OUT11   = '{0}/{1}'.format(args.output_dir, OUT11  )

    FTN11   = '{0}/{1}'.format(args.output_dir, FTN11  )
    FTN12   = '{0}/{1}'.format(args.output_dir, FTN12  )
    FTN13   = '{0}/{1}'.format(args.output_dir, FTN13  )
    FTN14   = '{0}/{1}'.format(args.output_dir, FTN14  )
    FTN15   = '{0}/{1}'.format(args.output_dir, FTN15  )
    FTN16   = '{0}/{1}'.format(args.output_dir, FTN16  )
    FTN17   = '{0}/{1}'.format(args.output_dir, FTN17  )
    FTN18   = '{0}/{1}'.format(args.output_dir, FTN18  )
    FTN19   = '{0}/{1}'.format(args.output_dir, FTN19  )
    FTN20   = '{0}/{1}'.format(args.output_dir, FTN20  )
    FTN21   = '{0}/{1}'.format(args.output_dir, FTN21  )
    FTN22   = '{0}/{1}'.format(args.output_dir, FTN22  )
    FTN23   = '{0}/{1}'.format(args.output_dir, FTN23  )

    SOF1    = '{0}/{1}'.format(args.output_dir, SOF1   )
    SOF2    = '{0}/{1}'.format(args.output_dir, SOF2   )

  if len(args.OPTPNM) > 0:
    OPTPNM  = args.OPTPNM
  if len(args.FTN15) > 0:
    FTN15  = args.FTN15
  if len(args.FTN16) > 0:
    FTN16  = args.FTN16


  # Argument list for arguments to be tagged onto the
  # end of the command.
  alist = []

  # Argument list for args to be added on to the end
  # of the qsub command.
  qargs = []
  
  # Build job script
  jobscr = ''
  #jobscr += '/usr/local/bin/qsub {0} - <<EOF'.format(' '.join(qargs))
  jobscr += '/bin/bash - <<EOF'
  jobscr += '\n#!/bin/bash'
  jobscr += '\n#PBS -q nas'
  jobscr += '\n#PBS -N {0}'.format(bname)
  jobscr += '\n#PBS -k n'
  jobscr += '\nexport SCRATCH_DIRECTORY={0}'.format(SCRATCH_DIRECTORY)
  jobscr += '\nexport RIGID_FORMAT_DIRECTORY={0}'.format(RIGID_FORMAT_DIRECTORY)

  jobscr += '\nexport NPTPNM={0}'.format(NPTPNM)
  jobscr += '\nexport PLTNM={0}'.format(PLTNM)
  jobscr += '\nexport DICTNM={0}'.format(DICTNM)
  jobscr += '\nexport PUNCHNM={0}'.format(PUNCHNM)
  jobscr += '\nexport OPTPNM={0}'.format(OPTPNM)
  jobscr += '\nexport LOGNM={0}'.format(LOGNM)

  jobscr += '\nexport IN12={0}'.format(IN12)
  jobscr += '\nexport OUT11={0}'.format(OUT11)

  jobscr += '\nexport FTN11={0}'.format(FTN11 )
  jobscr += '\nexport FTN12={0}'.format(FTN12 )
  jobscr += '\nexport FTN13={0}'.format(FTN13 )
  jobscr += '\nexport FTN14={0}'.format(FTN14 )
  jobscr += '\nexport FTN15={0}'.format(FTN15 )
  jobscr += '\nexport FTN16={0}'.format(FTN16 )
  jobscr += '\nexport FTN17={0}'.format(FTN17 )
  jobscr += '\nexport FTN18={0}'.format(FTN18 )
  jobscr += '\nexport FTN19={0}'.format(FTN19 )
  jobscr += '\nexport FTN20={0}'.format(FTN20 )
  jobscr += '\nexport FTN21={0}'.format(FTN21 )
  jobscr += '\nexport FTN22={0}'.format(FTN22 )
  jobscr += '\nexport FTN23={0}'.format(FTN23 )

  jobscr += '\nexport SOF1={0}'.format(SOF1)
  jobscr += '\nexport SOF2={0}'.format(SOF2)

  jobscr += '\nexport DBMEM={0}'.format(12000000)
  jobscr += '\nexport OCMEM={0}'.format(2000000)

  jobscr += '\ncd {0}'.format(workdir)
  jobscr += '\nmkdir -p {0}'.format(SCRATCH_DIRECTORY)
  jobscr += '\n{0} {1} <{2} >{3}'.format(nastran_x,
                                       ' '.join(alist),
                                           args.input,
                                           F06)
  jobscr += '\nrm -rf   {0}'.format(SCRATCH_DIRECTORY)
  jobscr += '\nEOF'

  if args.no_run:
    print(jobscr)
  else:
    os.system(jobscr)

def set_parser(parser):
  parser.set_defaults(func=main)

  parser.add_argument('--version',
                      action='version',
                      version='nesub-{0}'.format(__version__))

  parser.add_argument('-n', '--no-run',
                      action='store_true',
                      default=False,
                      help='print generated job script, but do not run')

  parser.add_argument('-O', '--OPTPNM',
                      metavar='OUTPNM',
                      type=str,
                      default='',
                      help='override OPTPNM (OutPut TaPe NuMber)')

  parser.add_argument('-f15', '--FTN15',
                      metavar='FTN15',
                      type=str,
                      default='',
                      help='override FTN15')

  parser.add_argument('-f16', '--FTN16',
                      metavar='FTN16',
                      type=str,
                      default='',
                      help='override FTN16')

  parser.add_argument('-o', '--output-dir',
                      metavar='OUTFILESPEC',
                      type=str,
                      default='',
                      help='absolute path of directory to write output')

  parser.add_argument('input',
                      metavar='INPUT',
                      type=str,
                      #nargs='+',
                      help='input file(s)')

if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  set_parser(parser)
  args = parser.parse_args()
  args.func(args)
