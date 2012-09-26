/* Used only for debugging the operand evaluator
   This prints the OpType, OpMtype (hex byte), and
   if it is a memory operand, it also prints the Base,
   Indx, Disp, and size if they exist.
   THIS IS NOT INCLUDED IN DELIVERABLE DASM.EXE
*/

void print_Oper(int op)
{
switch (rgOpType[op]) {
	case r32:
        fputs("r32= ", lst_fp);
		put_num(rgOpReg[op], lst_fp);
        break;
	case r16:
        fputs("r16= ", lst_fp);
		put_num(rgOpReg[op], lst_fp);
        break;
	case r8:
        fputs("r8= ", lst_fp);
		put_num(rgOpReg[op], lst_fp);
        break;
	case rCRG:
        fputs("rCRG= ", lst_fp);
		put_num(rgOpReg[op], lst_fp);
        break;
	case rDRG:
        fputs("rDRG= ", lst_fp);
		put_num(rgOpReg[op], lst_fp);
        break;
	case rTRG:
        fputs("rTRG= ", lst_fp);
		put_num(rgOpReg[op], lst_fp);
        break;
	case rSEG:
        fputs("rSEG= ", lst_fp);
		put_num(rgOpReg[op], lst_fp);
        break;

	case mem:		/* memory operand */
        fputs("MemOp- ", lst_fp);
        if (OpMType & fBase) {		/* Base register */
	        fputs(" Base-", lst_fp);
			put_num(rgOpBase[op], lst_fp);
			}
        if (OpMType & fIndx) {		/* Index Register */
	        fputs(" Indx-", lst_fp);
			put_num(rgOpIndx[op], lst_fp);
	        if (OpMType & fScale2) {		/* scale value 2 */
		        fputs(" Scale2 ", lst_fp);
				}
	        if (OpMType & fScale4) {		/* scale value 4 */
			    fputs(" Scale4 ", lst_fp);
				}
	        if (OpMType & fScale8) {		/* scale value 8 */
			     fputs(" Scale8 ", lst_fp);
				}
			}
        if (OpMType & fDisp8) {           /* Disp 8 value */
	        fputs(" Disp8-", lst_fp);
			put_hex(rgOpDisp[op], 2, lst_fp);
			}
        if (OpMType & fDisp32) {           /* Disp 32 value */
	        fputs(" Disp32-", lst_fp);
			put_hex(rgOpDisp[op], 8, lst_fp);
			}
		break;
	case val8:
        fputs("Val8=", lst_fp);
		put_num(rgOpDisp[op], lst_fp);
        break;
	case val16:
        fputs("Val16=", lst_fp);
		put_num(rgOpDisp[op], lst_fp);
        break;
	case val32:
        fputs("Val32=", lst_fp);
		put_num(rgOpDisp[op], lst_fp);
        break;
	default:
	    fputs(" UNK Operand ", lst_fp);
}
}
