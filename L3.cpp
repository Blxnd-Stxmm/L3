#include <iostream>
#include <algorithm>
#include <string>
#include <sstream>
#include <regex>
#include <cassert>
using namespace std;


void error(bool condition, int code, std::string msg);



class Array {
public:
    Array(int size = 0);
    Array(const Array& a);
    ~Array();

    virtual Array& operator=(const Array& right);

    virtual std::string toString() const;
    int size() const;
    static bool range(int value, int begin, int end);

    friend std::istream& operator>>(std::istream& in, const Array& right);
    int& operator[](int idx) const;
    int& operator[](int idx);
    virtual Array operator+(const Array& right) const;
    void resize(int new_size)
    {
        if (new_size < size_m)
            for (int i = new_size; i < size_m; i++)
                inf[i] = 0;
        size_m = new_size;
    }
protected:
    int size_m;
    int* inf;
    const static int MAX_SIZE = 50;
    void init();

};

std::ostream& operator<<(std::ostream& out, const Array& right);

class BitString : public Array {
public:
    BitString(int size = 0);
    BitString(std::string str);
    BitString(const BitString& bstr);
    ~BitString();

    virtual BitString& operator=(const BitString& right);

    virtual std::string toString() const;
    void fromString(std::string str);

    virtual BitString& operator>>(int right);
    virtual BitString& operator<<(int right);
    virtual BitString operator^(BitString right) const;
    virtual BitString operator&(BitString right) const;
    virtual BitString operator+(BitString right) const;
    virtual BitString operator~() const;

    friend std::istream& operator>>(std::istream& in, BitString& right);
};

class Hex : public Array
{
public:
    Hex(int size = 1) : Array(size) {}
    Hex(const Hex& hex) : Array() { *this = hex; }
    Hex(std::string str) : Array() { fromString(str); }
    ~Hex() {}

    int hex_char_to_int(int hex_char) const
    {
        return (hex_char <= '9' ? (hex_char - '0') : (hex_char - 'A' + 10));
    }
    char int_to_hex_char(int int_value) const
    {
        return (int_value <= 9 ? (int_value + '0') : (int_value + 'A' - 10));
    }

    virtual std::string toString() const
    {
        stringstream buf;
        if (is_negative) buf << "-";
        for (int i = size() - 1; i >= 0; --i) {
            buf << int_to_hex_char(inf[i]);
        }
        return buf.str();
    }
    void fromString(std::string str)
    {
        resize((unsigned int)str.size());
        for (int i = 0; i < size(); ++i)
            inf[i] = hex_char_to_int(str[str.size() - 1 - i]);
    }

    friend istream& operator>>(istream& in, Hex& a)
    {
        string buf;
        string str;
        for (int i = 0; i < a.size(); i++)
        {
            in >> buf;
            str += buf;
        }
        buf = "";
        for (int i = a.size() - 1; i >= 0; i--)
            buf.push_back(str[i]);
        a.fromString(buf);
        return in;
    }


    friend ostream& operator<<(ostream& out, Hex& a)
    {
        out << a.toString();
        return out;
    }

    static int compare(const Hex& a, const Hex& b)
    {
        for (int i = MAX_SIZE - 1; i >= 0; --i)
            if (a.inf[i] > b.inf[i])   return 1;
            else if (a.inf[i] < b.inf[i])   return -1;


        return 0;
    }

    friend bool operator==(const Hex& a, const Hex& b) { return Hex::compare(a, b) == 0; }
    friend bool operator!=(const Hex& a, const Hex& b) { return Hex::compare(a, b) != 0; }
    friend bool operator>=(const Hex& a, const Hex& b) { return Hex::compare(a, b) != -1; }
    friend bool operator<=(const Hex& a, const Hex& b) { return Hex::compare(a, b) != 1; }
    friend bool operator >(const Hex& a, const Hex& b) { return Hex::compare(a, b) == 1; }
    friend bool operator <(const Hex& a, const Hex& b) { return Hex::compare(a, b) == -1; }

    void remove_leading_zeros()
    {
        while (inf[size() - 1] == 0 && size() > 1)
            inf[--size_m] = 0;;
    }
    Hex& right_shift(int x)
    {
        if (*this == Hex())
            return *this;
        for (int i = size() - 1; i >= 0; --i)
            inf[i + x] = inf[i];
        for (int i = 0; i < x; ++i)
            inf[i] = 0;
        size_m += x;
        return *this;
    }
    Hex& left_shift(int x)
    {
        if (*this == Hex())
            return *this;
        for (int i = 0; i < size(); ++i)
            inf[i] = inf[i + x];
        for (int i = size() - x; i < size(); ++i)
            inf[i] = 0;
        size_m -= x;
        return *this;
    }
    virtual Hex operator+(Hex right) const
    {
        Hex c(max(this->size(), right.size()) + 1);
        for (int i = 0; i < max(this->size(), right.size()); ++i)
        {
            c.inf[i] += this->inf[i] + right.inf[i];
            c.inf[i + 1] += c.inf[i] / Hex::base;
            c.inf[i] %= Hex::base;
        }
        if (c.inf[c.size() - 1] == 0)
            c.size_m -= 1;
        return c;
    }
    friend Hex operator-(const Hex& a, const Hex& b)
    {

        if (a < b)
        {
            Hex c(b.size());
            c = b - a;
            c.is_negative = true;
            return c;
        }
        Hex c(a.size());
        for (int i = 0; i < a.size() + 1; ++i)
        {
            c.inf[i] += a.inf[i] - b.inf[i];
            if (c.inf[i] < 0) {
                c.inf[i] += Hex::base;
                c.inf[i + 1] -= 1;
            }
        }
        c.size_m = a.size();
        c.remove_leading_zeros();
        return c;
    }
    friend Hex operator*(const Hex& a, const Hex& b)
    {
        Hex c;
        for (int i = b.size() - 1; i >= 0; --i) {
            c.right_shift(1);
            c = c + (a * b.inf[i]);
        }
        return c;
    }
    friend Hex operator*(const Hex& a, const short digit)
    {
        Hex c(a.size());
        assert(digit >= 0 && digit < 16 && "hex_digit is not hex digit");
        for (int i = 0; i < a.size(); ++i)
        {
            c.inf[i] += (a.inf[i] * digit);
            c.inf[i + 1] += c.inf[i] / Hex::base;
            c.inf[i] %= Hex::base;
        }
        c.size_m = (c.inf[a.size()] == 0 ? a.size() : a.size() + 1);
        c.remove_leading_zeros();
        return c;
    }

    friend Hex operator/(const Hex& a, const Hex& b)
    {
        Hex result, sub;
        for (int i = 0; a.size() - i >= 0; ++i) {
            sub.right_shift(1);
            sub.inf[0] = a.inf[a.size() - i];
            result.right_shift(1);
            int count_division = 0;
            while (Hex::compare(sub, b) != -1) {
                sub = sub - b;
                ++count_division;
            }
            result.inf[0] = count_division;
        }
        return result;
    }

private:
    static const short base = 16;
    bool is_negative = false;
};

// Реализация Array
void error(bool condition, int code, string msg) {
    if (condition) {
        cerr << msg;
        exit(code);
    }
}

void Array::init() {
    for (int idx = 0; idx < MAX_SIZE + 1; ++idx)
        inf[idx] = 0;
}

bool Array::range(int value, int begin, int end) {
    return (begin <= value && value < end);
}


Array::Array(int size) : size_m(size), inf(nullptr) {
    if (inf == nullptr) {
        inf = new int[MAX_SIZE + 1];
    }
    init();
}

Array::Array(const Array& a) : size_m(0), inf(nullptr) {
    if (inf == nullptr) {
        inf = new int[MAX_SIZE + 1];
    }
    init();
    *this = a;
}

Array::~Array() {
    if (inf != nullptr) {
        delete[] inf;
    }
}

int Array::size() const {
    return size_m;
}

Array& Array::operator=(const Array& a) {
    if (this != &a) {
        if (a.size() < size_m) {
            for (int idx = a.size(); idx < size_m; ++idx) {
                inf[idx] = 0;
            }
        }
        size_m = a.size();
        for (int idx = 0; idx < size(); ++idx)
            inf[idx] = a[idx];
        size_m = a.size_m;
    }
    return *this;
}

int& Array::operator[](int idx) const {
    error(!range(idx, 0, size()), 3, "out of range");
    return inf[idx];
}

int& Array::operator[](int idx) {
    error(!range(idx, 0, size()), 3, "out of range");
    return inf[idx];
}

string Array::toString() const {
    stringstream buf;
    for (int i = 0; i < size(); ++i)
        buf << (int)inf[i] << " ";
    return buf.str();
}


istream& operator>>(istream& in, const Array& right) {
    for (int i = 0; i < right.size_m; ++i) {
        in >> right[i];
        //right[i] -= '0';
    }
    return in;
}


Array Array::operator+(const Array& right) const {
    Array result(max(size(), right.size()));
    for (int i = 0; i < result.size(); ++i)
        result.inf[i] = inf[i] + right.inf[i];
    return result;
}

ostream& operator<<(ostream& out, const Array& right) {
    if (right.toString() == "")
        out << "0";
    else
        out << right.toString();
    return out;
}

// Реализация BitString
BitString::BitString(int size) : Array(size) {}
BitString::BitString(string str) : Array() { fromString(str); }
BitString::BitString(const BitString& bstr) : Array() { *this = bstr; }
BitString::~BitString() {}

void BitString::fromString(string str) {
    str.erase(remove(str.begin(), str.end(), ' '), str.end());
    for (int i = 0; i < (int)str.size(); ++i) {
        inf[i] = str[i] - '0';
    }
    size_m = str.size();
}

BitString BitString::operator~() const {
    BitString result(*this);
    for (int i = 0; i < this->size(); ++i)
        result.inf[i] = !result.inf[i];
    return result;
}

BitString BitString::operator&(BitString right) const {
    BitString left = *this;
    left.size_m = right.size_m = max(left.size(), right.size());
    for (int i = 0; i < max(left.size(), right.size()); ++i)
        left.inf[i] = left.inf[i] & right.inf[i];
    return left;
}

BitString BitString::operator+(BitString right) const {
    BitString left = *this;
    left.size_m = right.size_m = max(left.size(), right.size());
    for (int i = 0; i < max(left.size(), right.size()); ++i)
        left.inf[i] = left.inf[i] || right.inf[i];
    return left;
}

BitString BitString::operator^(BitString right) const {
    BitString left = *this;
    left.size_m = right.size_m = max(left.size(), right.size());
    for (int i = 0; i < max(left.size(), right.size()); ++i)
        left.inf[i] = left.inf[i] != right.inf[i];
    return left;
}

BitString& BitString::operator>>(int right) {
    for (int i = size() - 1; i >= right; --i)
        inf[i] = inf[i - right];
    for (int i = right - 1; i >= 0; --i)
        inf[i] = 0;
    return *this;
}

BitString& BitString::operator<<(int left) {
    for (int i = 0; i < size() - left; ++i)
        inf[i] = inf[i + left];
    for (int i = size() - left; i < size(); ++i)
        inf[i] = 0;

    return *this;
}

string BitString::toString() const {
    stringstream buf;
    for (int i = 0; i < size(); ++i)
        buf << (int)inf[i];
    return buf.str();
}

istream& operator>>(istream& in, BitString& right) {
    string str;
    string buf;
    for (int i = 0; i < right.size(); i++)
    {
        in >> buf;
        str += buf;
    }

    right.fromString(str);
    return in;
}

BitString& BitString::operator=(const BitString& right) {
    if (this != &right) {
        if (right.size() < size_m) {
            for (int idx = right.size(); idx < size_m; ++idx) {
                inf[idx] = 0;
            }
        }
        size_m = right.size();
        for (int idx = 0; idx < size(); ++idx)
            inf[idx] = right[idx];
        size_m = right.size_m;
    }
    return *this;
}

void Array_Print(Array per1, Array per2)
{
    cout << "Array1 " << per1 << "Array2 " << per2 << endl;
    cout << "Array1+Array2 " << per1 + per2 << endl;
}

void Hex_Print(Hex per1, Hex per2)
{
    cout << "Hex1 " << per1 << " Hex2 " << per2 << endl;

    string amount = (per1 + per2).toString();
    string difference = (per1 - per2).toString();
    string prod = (per1 * per2).toString();
    string division = (per1 / per2).toString();
    int prod_i = prod.length() - 16;
    if (prod[prod_i] == '0') prod_i++;
    for (auto& symbol : amount)
        symbol = tolower(symbol);
    for (auto& symbol : difference)
        symbol = tolower(symbol);
    for (auto& symbol : prod)
        symbol = tolower(symbol);
    for (auto& symbol : division)
        symbol = tolower(symbol);

    cout << "Hex1+Hex2  " << amount << endl;
    cout << "Hex1-Hex2  " << difference << endl;
    cout << "Hex1*Hex2  " << prod.substr(prod_i) << endl;
    cout << "Hex1/Hex2  " << division << endl;
}

void BitString_Print(BitString per1, BitString per2)
{
    cout << "BitString1 " << per1 << " BitString2 " << per2 << endl;
    cout << "BitString1 & BitString2 " << (per1 & per2) << endl;
    cout << "BitString1 ! BitString2 " << per1 + per2 << endl;
    cout << "BitString1 ^ BitString2 " << (per1 ^ per2) << endl;
    cout << "! BitString1 " << ~(per1) << " ! BitString2 " << ~(per2) << endl;

}

void main()
{
    int size_arr = 0,
        size_h = 0,
        size_bitstring = 0;

    cin >> size_arr;

    Array per1(size_arr);
    Array per2(size_arr);
    cin >> per1;
    cin >> per2;
    Array_Print(per1, per2);

    cin >> size_h;

    Hex hex1(size_h);
    Hex hex2(size_h);
    cin >> hex1;
    cin >> hex2;
    Hex_Print(hex1, hex2);

    cin >> size_bitstring;

    BitString per3(size_bitstring);
    BitString per4(size_bitstring);
    cin >> per3;
    cin >> per4;
    BitString_Print(per3, per4);

    int code = -1;
    int number = 0;
    while (code != 0)
    {
        cin >> code;
        switch (code)
        {
        case 1:
        {
            cin >> number;
            cin >> per1[number];
            Array_Print(per1, per2);
            break;
        }
        case 2:
        {
            cin >> number;
            cin >> per2[number];
            Array_Print(per1, per2);
            break;
        }
        case 3:
        {
            cin >> hex1;
            Hex_Print(hex1, hex2);
            break;
        }
        case 4:
        {
            cin >> hex2;
            Hex_Print(hex1, hex2);
            break;
        }
        case 5:
        {
            cin >> per3;
            BitString_Print(per3, per4);
            break;
        }
        case 6:
        {
            cin >> per4;
            BitString_Print(per3, per4);
            break;
        }
        }
    }
    //return 0;
}