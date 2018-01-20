#include <iostream>
#include <cmath>
#include <limits>
using namespace std;

#define NaN std::numeric_limits<float>::quiet_NaN()
#define isNaN(X) (X != X) // NaN is the only float that is not equal to itself

enum RorI {re, im};
enum charstate {leadingsign, predeci, point, deci, chari};

enum operation {add, mul, divi, sub, to};
enum order {normal, prior, topprior};
enum insidebracket {no, yes};
enum errortype {noerror, syntax, math};
enum command {esc, polar, standard, nocommand};

//class for complex number
class C {

public:
  C (){}
  C (double r, double i);
  C (const C& x); //copy: constructor overloading
  bool operator == (const C& x);

  C& operator = (const C& x);//operator overloading
  C operator + (const C& x);//operator overloading
  C operator - (const C& x);//operator overloading
  C operator * (const C& x);//operator overloading
  C operator / (const C& x);//operator overloading
  friend C calculator (const char *line, int& index, C l, bool cont, insidebracket state, order level);
  friend bool getC (const char *line, int& index, C& complex);
  friend errortype parseline (const char *line, C& c);
  friend C powerofcomplex (C base, C exponent);
  void printC ();
  void printPolarForm();
  void printExponentialForm();

private:
    double real;
    double imaginary;
};

command parsecommand (char *line);


/////////////////////////////////////////////////////////////////////////////////////////////

//recursive structure for calculator
C calculator (const char *line, int& index, C l, bool cont, insidebracket state, order level){
  C left(0,0);
  C right(0,0);
  C num(0,0);
  C syntaxerror (NaN, 0);
  C matherror (0, NaN);
  if (cont){
    left=l;
  }
  else {
    if (line[index]>='0'&&line[index]<='9'||line[index]=='-'||line[index]=='+'||line[index]=='i'){
      if (!getC(line, index, left)){
        return syntaxerror;
      }
    }
    else if (line[index]=='('){
      index++;
      left=calculator(line, index, left , false , yes , normal);
    }
    else {
      return syntaxerror;
    }
  }

  if (line[index]==0){
    return left;
  }

  if (line[index]==')'){
    if (state==yes){
      index++;
      return left;
    }
    else {
      return syntaxerror;
    }
  }

  //error check
  if (isNaN(left.real))
    return syntaxerror;
  if (isNaN(left.imaginary))
    return matherror;

  operation type;
  switch (line[index]){
    case '+':
      type=add;
      index++;
      break;
    case '-':
      type=sub;
      index++;
      break;
    case '*':
      type=mul;
      index++;
      break;
    case '/':
      type=divi;
      index++;
      break;
    case '^':
      type=to;
      index++;
      break;
    case '(':
      type=mul;
      break;
    default:
      return syntaxerror;
      break;
  }

  if (line[index]>='0'&&line[index]<='9'||line[index]=='-'||line[index]=='+'||line[index]=='i'){
    if (!getC(line, index, right)){
      return syntaxerror;
    }
  }
  else if (line[index]=='('){
    index++;
    right=calculator(line, index, left, false , yes, normal);
  }
  //error check
  if (isNaN(right.real))
    return syntaxerror;
  if (isNaN(left.imaginary))
    return matherror;

  //key: identify the priority of the operation types
  if (type==add||type==sub){
    if (line[index]=='*'||line[index]=='/'||line[index]=='^'||line[index]=='('){
      right=calculator(line, index, right, true, no, prior);
    }
  }
  else if (type==divi||type==mul){
    if (line[index]=='^'){
      right=calculator(line, index, right, true, no, topprior);
    }
  }

  //actual operations
  switch (type){
    case add:
      num=left+right;
      break;

    case sub:
      num=left-right;
      break;

    case mul:
      num=left*right;
      break;

    case divi:
      if (right==num)
        return matherror;
      num=left/right;
      break;

    case to:
      if (right.imaginary!=0)
        return matherror;
      num=powerofcomplex(left,right);
      break;
  }
  //key: different conditions for return value in different scenarios
  if (state==yes){
    if (line[index]==0)
      return num;
    if (line[index]==')'){
      index++;
      return num;
    }
    if (level==topprior){
      if (line[index]=='+'||line[index]=='-'||line[index]=='*'||line[index]=='/'||line[index]=='(')
        return num;
      if (line[index]=='^')
        return calculator (line, index, num, true, yes, topprior);
      else {
        return syntaxerror;
      }
    }
    else if (level==prior){
      if (line[index]=='+'||line[index]=='-')
        return num;
      if (line[index]=='*'||line[index]=='/'||line[index]=='^'||line[index]=='('){
        return calculator (line, index, num, true, yes, prior);
      }
      else {
        return syntaxerror;
      }
    }
    else{
      if (line[index]=='+'||line[index]=='-'||line[index]=='*'||line[index]=='/'||line[index]=='^'||line[index]=='('){
        return calculator (line, index, num, true, yes, normal);
      }
      else {
        return syntaxerror;
      }
    }
  }
  // not inside the bracket
  else {
    if (line[index]==0)
      return num;
    if (level==topprior){
      if (line[index]=='+'||line[index]=='-'||line[index]=='*'||line[index]=='/'||line[index]=='('||line[index]==')')
        return num;
      if (line[index]=='^')
        return calculator (line, index, num, true, no , topprior);
      else {
        return syntaxerror;
      }
    }
    if (level==prior){
      if (line[index]=='+'||line[index]=='-'||line[index]==')')
        return num;
      if (line[index]=='*'||line[index]=='/'||line[index]=='^'||line[index]=='('){
        return calculator (line, index, num, true, no , prior);
      }
      else {
        return syntaxerror;
      }
    }
    else {
      if (line[index]=='+'||line[index]=='-'||line[index]=='*'||line[index]=='/'||line[index]=='^'||line[index]=='('){
        return calculator (line, index, num, true, no , normal);
      }
      else {
        return syntaxerror;
      }
    }
  }
}

errortype parseline (const char *line, C& c){
  int i=0;
  C num (0,0);
  c=calculator(line, i, num, false, no , normal);
  if (isNaN(c.real))
    return syntax;
  if (isNaN(c.imaginary))
    return math;
  return noerror;
}

//functions of class
C::C (double r, double i){
  real=r;
  imaginary=i;
}

C::C (const C& x){
  real=x.real;
  imaginary=x.imaginary;
}

bool C::operator == (const C& x){
  if (this->real == x.real && this->imaginary == x.imaginary)
    return true;
  return false;
}

C& C::operator = (const C& x){
  this->real=x.real;
  this->imaginary=x.imaginary;
  return *this;
}

C C::operator + (const C& x){
  double r=this->real+x.real;
  double i=this->imaginary+x.imaginary;
  C tmp (r, i);
  return tmp;
}

C C::operator - (const C& x){
  double r=this->real-x.real;
  double i=this->imaginary-x.imaginary;
  C tmp (r, i);
  return tmp;
}

C C::operator * (const C& x){
  double r=(this->real*x.real-this->imaginary*x.imaginary);
  double i=(this->real*x.imaginary+this->imaginary*x.real);
  C tmp (r, i);
  return tmp;
}

C C::operator / (const C& x){
  double denominator=x.real*x.real+x.imaginary*x.imaginary;
  double r=(this->real*x.real+this->imaginary*x.imaginary)/denominator;
  double i=(this->imaginary*x.real-this->real*x.imaginary)/denominator;
  C tmp (r, i);
  return tmp;
}

void C::printC (){
  if (fabs(real)<=0.0000000001&&fabs(imaginary)>0.0000000001){
    if (imaginary!=1)
      cout<<imaginary;
    cout<<"i"<<endl;
    return;
  }
  cout<<real;
  if (imaginary>0){
    if (imaginary==1)
      cout<<"+i";
    else
      cout<<"+"<<imaginary<<"i";
  }
  else if (imaginary<0){
    if (imaginary==-1)
      cout<<"-i";
    else
      cout<<"-"<<-1*imaginary<<"i";
  }
  cout<<endl;
}

void C::printExponentialForm(){
  double modulus=sqrt(real*real+imaginary*imaginary);
  double theta=acos(real/modulus);
  if (imaginary<0)
    theta=2*3.14159265-theta;
  cout<<modulus<<"*e^"<<theta<<"i"<<endl;
}

void C::printPolarForm(){
  double modulus=sqrt(real*real+imaginary*imaginary);
  double theta=acos(real/modulus);
  if (imaginary<0)
    theta=2*3.14159265-theta;
  cout<<modulus<<"*(cos"<<theta<<"+i*sin"<<theta<<")"<<endl;
}

C powerofcomplex (C base, C exponent){
  double ex=exponent.real;
  double modulus=sqrt(base.real*base.real+base.imaginary*base.imaginary);
  double theta=acos(base.real/modulus);
  if (base.imaginary<0)
    theta=2*3.14159265-theta;
  modulus=pow(modulus,ex);
  theta*=ex;
  double real=modulus*cos(theta);
  double imaginary=modulus*sin(theta);
  C c(real, imaginary);
  return c;
}
//functions of class end

//state machine function to parse user input
bool getC (const char *line, int& index, C& complex){
  double num=0;
  int sign=1;
  int ex=1;
  RorI rori=re;
  charstate state;
  //initialize state machine
  if (line[index]=='+'||line[index]=='-'){
    state=leadingsign;
    if (line[index]=='-')
      sign*=-1;
  }
  else if (line[index]>='0'&&line[index]<='9'){
    state=predeci;
    num=num*10+(double)(line[index]-'0');
  }
  else if (line[index]=='.'){
    state=point;
  }
  else if (line[index]=='i'){
    state=chari;
    rori=im;
  }
  else{
    return false;
  }

  //state machine starts here

  bool done=false;

  do{
    index++;
    switch (state){
      case leadingsign:
        if (line[index]=='+'||line[index]=='-'){
          state=leadingsign;
          if (line[index]=='-')
            sign*=-1;
        }
        else if (line[index]>='0'&&line[index]<='9'){
          state=predeci;
          num=num*10+(double)(line[index]-'0');
        }
        else if (line[index]=='.'){
          state=point;
        }
        else if (line[index]=='i'){
          state=chari;
          rori=im;
        }
        else{
          return false;
        }
        break;

      case predeci:
        if (line[index]>='0'&&line[index]<='9'){
          state=predeci;
          num=num*10+(double)(line[index]-'0');
        }
        else if (line[index]=='.'){
          state=point;
        }
        else if (line[index]=='i'){
          if (rori==im)
            return false;
          rori=im;
          index++;
          done=true;
        }
        else{
          done=true;
        }
        break;

      case point:
        if (line[index]>='0'&&line[index]<='9'){
          state=deci;
          num=num*10+(double)(line[index]-'0');
          ex*=10;
        }
        else{
          return false;
        }
        break;

      case deci:
        if (line[index]>='0'&&line[index]<='9'){
          state=deci;
          num=num*10+(double)(line[index]-'0');
          ex*=10;
        }
        else if (line[index]=='i'){
          if (rori==im)
            return false;
          rori=im;
          index++;
          done=true;
        }
        else{
          done=true;
        }
        break;

      case chari:
        if (line[index]>='0'&&line[index]<='9'){
          state=predeci;
          num=num*10+(double)(line[index]-'0');
        }
        else if (line[index]=='.'){
          state=point;
        }
        else {
          complex.real=0;
          complex.imaginary=1;
          return true;
        }
        break;

      default:
        return false;
    }
  } while (!done);

  if (rori==re){
    complex.real=(double)sign*num/(double)ex;
    complex.imaginary=0;
  }
  else {
    complex.imaginary=(double)sign*num/(double)ex;
    complex.real=0;
  }
  return true;
}

command parsecommand (char *line){
  if (line[0]=='e'&&line[1]=='s'&&line[2]=='c'&&line[3]==0)
    return esc;
  if (line[0]=='P'&&line[1]==0)
    return polar;
  if (line[0]=='S'&&line[1]==0)
    return standard;
  else
    return nocommand;
}



int main(){
  cout<<"Simple ComplexNumber Calculator (version 1.0) by Xinzhe Wang"<<endl;
  cout<<"Instruction:"<<endl;
  cout<<"Enter 'S' to change display form to standard form (default)"<<endl;
  cout<<"Enter 'P' to change display form to polar form (angle in radians)"<<endl;
  cout<<"Enter 'esc' to exit the program"<<endl;
  cout<<"Enter the formula to calculate the result (use i for the imaginary part)"<<endl;
  cout<<"Example: (1+i)*(1-i)-2"<<endl;
  cout<<"(Note: power function with complex number as exponent is considered as math error in this program)"<<endl;
  cout<<endl;
  bool esc=false;
  command Ctype=standard;
  errortype Etype;
  while (!esc){
    cout<<"Please enter your equation or command:"<<endl;
    char line[100];
    cin.getline(line,100);
    command commandtype=parsecommand(line);
    if (commandtype==esc)
      return 0;
    else if (commandtype==polar){
      Ctype=polar;
      cout<<"Display mode is in Polar now"<<endl;
      cout<<endl;
    }
    else if (commandtype==standard){
      Ctype=standard;
      cout<<"Display mode is in Standard now"<<endl;
      cout<<endl;
    }
    else if (commandtype==nocommand){
      C c(0,0);
      Etype=parseline(line, c);
      if (Etype==syntax)
        cout<<"Syntax Error"<<endl;
      else if (Etype==math)
        cout<<"Math Error"<<endl;
      else {
        switch (Ctype){
          case standard:
            cout<<"=";
            c.printC();
            cout<<endl;
            break;
          case polar:
            cout<<"=";
            c.printPolarForm();
            cout<<endl;
            break;
        }
      }
    }
  }
}
