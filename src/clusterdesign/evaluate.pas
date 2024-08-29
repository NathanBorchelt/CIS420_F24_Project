{
Copyright (C) 2012 Konstantin S. Solnushkin

This file is part of "dbcli", the tool to query graph-based databases.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
}
unit Evaluate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Common, FloatStr;                         // Our own units

type
  TCharSet = set of Char;

const
  // Strings
  cPlus = '+';
  // Character sets
  Operators: TCharSet = [cPlus, '-', '*', '/'];
  Digits: TCharSet = ['0', '1'..'9'];
  DecimalSeps: TCharSet = ['.', ','];
  Braces: TCharSet = ['(', ')'];
  Space: TCharSet = [cSpace];

function EvaluateExpression (Expression: String): String;

implementation

function EvaluateExpression (Expression: String): String;
// This sub-function calls itself resursively. All in all, the number of calls is
// the number of opening parenthesis in your expression, plus one.

  function FindLeft (const Expression: String; const Idx: Integer): String;
  // Parse string to the left of position "Idx" until a non-digit is found
  var
    I: Integer;
  begin
    // Start from the character previous to "Idx"
    I := Idx - 1;

    // While it is still part of a number, move left
    while (I > 0) and (Expression[I] in Digits + DecimalSeps) do
      I -= 1;

    // Return one character to the right
    I += 1;
    // Now, the number of interest is between "I" and "Idx-1"; return it
    Result := Copy (Expression, I, Idx - I);
  end;

  function FindRight (const Expression: String; const Idx: Integer): String;
  // Parse string to the right of position "Idx" until a non-digit is found
  var
    I: Integer;
  begin
    // Start from the character following "Idx"
    I := Idx + 1;
    // While it is still part of a number, move right
    while (I < Length (Expression)) and (Expression[I] in Digits + DecimalSeps) do
      I += 1;
    // If we stopped NOT because it was the end of the string, return one character to the left
    if not (I = Length (Expression)) then
      I -= 1;
    // Now, the number of interest is between "Idx+1" and "I"; return it
    Result := Copy (Expression, Idx + 1, I - Idx);
  end;

var
  Idx, IdxDiv, IdxMul, IdxPlus, IdxMinus: Integer;      // Indices where the operator is found
  I, J: Integer;                                        // Just indices
  NumLeft, NumRight: Real;                              // Numbers to the left and right of the operator
  V: Real;                                              // Result of arithmetic evaluation
  SubResult: String;                                    // Result of evaluation of a parenthesized sub-expression
  NumLeftStr, NumRightStr: String;                      // Numbers, when they are still stored in strings
begin

  // Remove spaces, if any
  while True do
  begin
    I := Pos (cSpace, Expression);
    if I = 0 then Break;
    System.Delete (Expression, I, 1);
  end;

  // In case we were accidentally passed an empty string:
  if Expression = '' then Exit;

  // The foremost task is to deal with parentheses
  while True do
  begin
    // Here is the strategy:
    // 1. Find the first _closing_ bracket
    // 2. Track back to find the corresponding opening bracket
    // 3. Evaluate the sub-expression in them
    // 4. Replace this sub-expression with the result

    I := Pos (')', Expression);
    if I = 0 then            // It is likely that no closing parentheses was found
    // But we need yet one more important check: no opening parentheses can be present as well
    begin
      Assert (Pos ('(', Expression) = 0, 'Malformed expression: unbalanced opening parenthesis: ' + Expression);
      Break;   // If all is well, we can safely exit
    end;

    // Start from the previous character
    J := I - 1;

    // Move left until an opening bracket is found, or start of the string
    while (J > 0) and (Expression[J] <> '(') do
      J -= 1;

    // There must be an opening parenthesis under the position "J":
    Assert (Expression[J] = '(', 'Malformed expression: missing opening parenthesis: ' + Expression);

    // Now, the sub-expression is between "J" and "I" -- evaluate the contents inside the parentheses:
    SubResult := EvaluateExpression (Copy (Expression, J + 1, I - J - 1));

    // Delete the parenthesized expression
    System.Delete (Expression, J, I - J + 1);
    // Insert the result of our evaluation
    System.Insert (SubResult, Expression, J);
  end;

  // Let's now handle "/" and "*" operations
  while True do
  begin
    // Find positions of "/" and "*":
    IdxDiv := Pos ('/', Expression);
    IdxMul := Pos ('*', Expression);

    if IdxDiv > 0 then
      Idx := IdxDiv                 // Do "/" first to avoid overflows
    else
      if IdxMul > 0 then            // "/" not found, but "*" found
        Idx := IdxMul
    else
      Break;                        // Neither operator found

    // If we reached here, then "Idx" contains the index of the operator's position in the string
    NumLeftStr := FindLeft (Expression, Idx);
    Assert (NumLeftStr <> '', 'Malformed expression: ' + Expression);
    NumRightStr := FindRight (Expression, Idx);
    Assert (NumRightStr <> '', 'Malformed expression: ' + Expression);

    // Convert to floats
    NumLeft := GenericStrToFloat (NumLeftStr);
    NumRight := GenericStrToFloat (NumRightStr);

    // Perform operation stipulated by the sign
    if Expression [Idx] = '/' then  // "/"
      V := NumLeft / NumRight
    else                            // "*"
      V := NumLeft * NumRight;

    // The last thing is to replace the sub-expression with the calculated result
    // ("1" is for the operator character itself)
    System.Delete (Expression, Idx - Length (NumLeftStr), Length (NumLeftStr) + 1 + Length (NumRightStr));
    System.Insert (GenericFloatToStr (V), Expression, Idx - Length (NumLeftStr));
  end;

  // Proceed with the lowest-priority "+" and "-" operations
  while True do
  begin
    // Find positions of "+" and "-":
    IdxPlus := Pos ('+', Expression);
    IdxMinus := Pos ('-', Expression);

    if IdxMinus > 0 then
      Idx := IdxMinus               // Do "-" first to avoid overflows (but is it really necessary?)
    else
      if IdxPlus > 0 then           // "-" not found, but "+" found
        Idx := IdxPlus
    else
      Break;                        // Neither operator found

    // If we reached here, then "Idx" contains the index of the operator's position in the string
    NumLeftStr := FindLeft (Expression, Idx);
    Assert (NumLeftStr <> '', 'Malformed expression: ' + Expression);
    // XXX: Unary minus? No, I haven't heard of such thing. (Note: will require a substational re-design of the parser)
    // Maybe use state machines? They are fancy.
    NumRightStr := FindRight (Expression, Idx);
    Assert (NumRightStr <> '', 'Malformed expression: ' + Expression);

    // Convert to floats
    NumLeft := GenericStrToFloat (NumLeftStr);
    NumRight := GenericStrToFloat (NumRightStr);

    // Perform operation stipulated by the sign
    if Expression [Idx] = '-' then  // "-"
      V := NumLeft - NumRight
    else                            // "+"
      V := NumLeft + NumRight;

    // The last thing is to replace the sub-expression with the calculated result
    // ("1" is for the operator character itself)
    System.Delete (Expression, Idx - Length (NumLeftStr), Length (NumLeftStr) + 1 + Length (NumRightStr));
    System.Insert (GenericFloatToStr (V), Expression, Idx - Length (NumLeftStr));
  end;

  // Return the updated expression
  Result := Expression;
end;

end.

