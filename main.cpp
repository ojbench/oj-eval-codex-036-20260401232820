// Allowed headers only
#include <vector>
#include <cstring>
#include <cstdint>
#include <iostream>
#include <algorithm>
#include <string>
using namespace std;

struct dynamic_bitset {
    using word_t = unsigned long long;
    static constexpr size_t WORD_BITS = 64;
    vector<word_t> data;
    size_t nbits = 0;

    dynamic_bitset() = default;
    ~dynamic_bitset() = default;
    dynamic_bitset(const dynamic_bitset&) = default;
    dynamic_bitset& operator=(const dynamic_bitset&) = default;

    dynamic_bitset(size_t n) { resize_bits(n); }
    dynamic_bitset(const string &str) {
        nbits = str.size();
        data.assign((nbits + WORD_BITS - 1)/WORD_BITS, 0);
        for (size_t i = 0; i < nbits; ++i) if (str[i] == '1') set(i, true);
    }

    size_t size() const { return nbits; }
    bool operator[](size_t n) const {
        if (n >= nbits) return false;
        size_t wi = n / WORD_BITS, bi = n % WORD_BITS;
        return (data[wi] >> bi) & 1ULL;
    }
    dynamic_bitset& set(size_t n, bool val = true) {
        if (n >= nbits) return *this;
        size_t wi = n / WORD_BITS, bi = n % WORD_BITS;
        if (val) data[wi] |= (1ULL << bi);
        else data[wi] &= ~(1ULL << bi);
        return *this;
    }
    dynamic_bitset& push_back(bool val) {
        ensure_capacity(nbits + 1);
        size_t wi = nbits / WORD_BITS, bi = nbits % WORD_BITS;
        if (val) data[wi] |= (1ULL << bi);
        else data[wi] &= ~(1ULL << bi);
        ++nbits;
        return *this;
    }
    bool none() const {
        if (nbits == 0) return true;
        for (size_t i = 0; i + 1 < data.size(); ++i) if (data[i]) return false;
        if (!data.empty()) {
            word_t w = data.back();
            size_t rem = nbits % WORD_BITS;
            if (rem) w &= ((1ULL<<rem)-1);
            return w == 0;
        }
        return true;
    }
    bool all() const {
        if (nbits == 0) return true;
        size_t full_words = nbits / WORD_BITS;
        size_t rem = nbits % WORD_BITS;
        for (size_t i = 0; i < full_words; ++i) if (~data[i]) return false;
        if (rem) {
            word_t mask = (rem == 64 ? ~0ULL : ((1ULL << rem) - 1));
            return (data[full_words] & mask) == mask;
        }
        return true;
    }
    dynamic_bitset& operator|=(const dynamic_bitset &o) {
        size_t len = min(nbits, o.nbits);
        size_t lw = len / WORD_BITS;
        size_t lr = len % WORD_BITS;
        for (size_t i = 0; i < lw; ++i) data[i] |= o.data[i];
        if (lr) {
            word_t mask = (1ULL << lr) - 1;
            data[lw] = (data[lw] & ~mask) | ((data[lw] | o.data[lw]) & mask);
        }
        return *this;
    }
    dynamic_bitset& operator&=(const dynamic_bitset &o) {
        size_t len = min(nbits, o.nbits);
        size_t lw = len / WORD_BITS;
        size_t lr = len % WORD_BITS;
        for (size_t i = 0; i < lw; ++i) data[i] &= o.data[i];
        if (lr) {
            word_t mask = (1ULL << lr) - 1;
            data[lw] = (data[lw] & ~mask) | ((data[lw] & o.data[lw]) & mask);
        }
        return *this;
    }
    dynamic_bitset& operator^=(const dynamic_bitset &o) {
        size_t len = min(nbits, o.nbits);
        size_t lw = len / WORD_BITS;
        size_t lr = len % WORD_BITS;
        for (size_t i = 0; i < lw; ++i) data[i] ^= o.data[i];
        if (lr) {
            word_t mask = (1ULL << lr) - 1;
            data[lw] = (data[lw] & ~mask) | ((data[lw] ^ o.data[lw]) & mask);
        }
        return *this;
    }
    dynamic_bitset& operator<<=(size_t n) {
        if (n == 0) return *this;
        size_t old_n = nbits;
        size_t new_bits = old_n + n;
        size_t word_shift = n / WORD_BITS;
        size_t bit_shift = n % WORD_BITS;
        size_t old_words = (old_n + WORD_BITS - 1) / WORD_BITS;
        ensure_capacity(new_bits);
        size_t new_words = (new_bits + WORD_BITS - 1) / WORD_BITS;
        data.resize(new_words, 0);
        if (bit_shift == 0) {
            for (size_t i = old_words; i-- > 0; ) data[i + word_shift] = data[i];
        } else {
            for (size_t i = old_words; i-- > 0; ) {
                word_t lo = data[i] << bit_shift;
                word_t hi = (i ? (data[i-1] >> (WORD_BITS - bit_shift)) : 0ULL);
                data[i + word_shift] = lo | hi;
            }
        }
        for (size_t i = 0; i < word_shift; ++i) data[i] = 0;
        nbits = new_bits;
        trim_to_nbits();
        return *this;
    }
    dynamic_bitset& operator>>=(size_t n) {
        if (n == 0) return *this;
        if (n >= nbits) { nbits = 0; data.clear(); return *this; }
        size_t word_shift = n / WORD_BITS;
        size_t bit_shift = n % WORD_BITS;
        size_t old_words = data.size();
        if (bit_shift == 0) {
            for (size_t i = 0; i + word_shift < old_words; ++i) data[i] = data[i + word_shift];
        } else {
            for (size_t i = 0; i + word_shift < old_words; ++i) {
                word_t lo = data[i + word_shift] >> bit_shift;
                word_t hi = (i + word_shift + 1 < old_words) ? (data[i + word_shift + 1] << (WORD_BITS - bit_shift)) : 0ULL;
                data[i] = lo | hi;
            }
        }
        nbits -= n;
        data.resize((nbits + WORD_BITS - 1) / WORD_BITS);
        trim_to_nbits();
        return *this;
    }
    dynamic_bitset& set() {
        if (nbits == 0) return *this;
        size_t words = (nbits + WORD_BITS - 1) / WORD_BITS;
        data.assign(words, ~0ULL);
        trim_to_nbits();
        return *this;
    }
    dynamic_bitset& flip() {
        for (auto &w : data) w = ~w;
        trim_to_nbits();
        return *this;
    }
    dynamic_bitset& reset() {
        std::fill(data.begin(), data.end(), 0ULL);
        return *this;
    }
private:
    void resize_bits(size_t n) {
        nbits = n;
        data.assign((nbits + WORD_BITS - 1) / WORD_BITS, 0);
        trim_to_nbits();
    }
    void ensure_capacity(size_t need_bits) {
        size_t need_words = (need_bits + WORD_BITS - 1) / WORD_BITS;
        if (data.size() < need_words) data.resize(need_words, 0);
    }
    void trim_to_nbits() {
        if (data.empty()) return;
        data.resize((nbits + WORD_BITS - 1) / WORD_BITS);
        if (!data.empty()) {
            word_t w = data.back();
            size_t rem = nbits % WORD_BITS;
            if (rem) w &= ((1ULL<<rem)-1);
            data.back() = w;
        }
    }
};

static string to_string_lsb(const dynamic_bitset &d){
    string s; s.resize(d.size());
    for(size_t i=0;i<d.size();++i) s[i] = d[i] ? '1':'0';
    return s;
}

int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    dynamic_bitset d;
    string cmd;
    if(!(cin>>cmd)) return 0;
    do{
        if(cmd==init){
            string s; if(!(cin>>s)) break;
            bool isnum = !s.empty() && all_of(s.begin(), s.end(), ::isdigit);
            if(isnum){ size_t n = stoull(s); d = dynamic_bitset(n); }
            else{ d = dynamic_bitset(s); }
        }else if(cmd==get){
            size_t i; cin>>i; cout<<(d[i]?1:0) << '\n';
        }else if(cmd==set){
            size_t i; int v; cin>>i>>v; d.set(i, v!=0);
        }else if(cmd==push){
            int v; cin>>v; d.push_back(v!=0);
        }else if(cmd==none){
            cout<<(d.none()?true:false)<<'\n';
        }else if(cmd==all){
            cout<<(d.all()?true:false)<<'\n';
        }else if(cmd==size){
            cout<<d.size()<<'\n';
        }else if(cmd==or){
            string s; cin>>s; dynamic_bitset o(s); d |= o;
        }else if(cmd==and){
            string s; cin>>s; dynamic_bitset o(s); d &= o;
        }else if(cmd==xor){
            string s; cin>>s; dynamic_bitset o(s); d ^= o;
        }else if(cmd==shl){
            size_t n; cin>>n; d <<= n;
        }else if(cmd==shr){
            size_t n; cin>>n; d >>= n;
        }else if(cmd==flip){
            d.flip();
        }else if(cmd==reset){
            d.reset();
        }else if(cmd==setall){
            d.set();
        }else if(cmd==print){
            cout<<to_string_lsb(d)<<'\n';
        }
    }while(cin>>cmd);
    return 0;
}

