import Result "mo:base/Result";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Hash "mo:base/Hash";
import Option "mo:base/Option";
import Types "types";
actor {

    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;

    let ledger = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);
    let tokenNameString : Text = "TalentToken";
    let tokenSymbolString : Text = "TAL";

    public query func tokenName() : async Text {
        return tokenNameString;
    };

    public query func tokenSymbol() : async Text {
        return tokenSymbolString;
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        switch (ledger.get(owner)) {
            case (?balance) {
                ledger.put(owner, amount + balance);
            };
            case (null) {
                ledger.put(owner, amount);
            };
        };
        return #ok();

        // let balanceOwner = Option.get(ledger.get(owner),0);
        // ledger.put(owner, balanceOwner + amount);
        // return #ok();
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balanceOwner = Option.get(ledger.get(owner), 0);
        if (balanceOwner < amount) {
            return #err("Not enough funds");
        } else {
            ledger.put(owner, balanceOwner - amount);
            return #ok();
        };
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        switch (await burn(from, amount)) {
            case (#ok()) {
                return await mint(to, amount);
            };
            case (#err(msg)) {
                return #err(msg);
            };
        };

        return #ok();
    };

    public query func balanceOf(account : Principal) : async Nat {
        return  Option.get(ledger.get(account),0);
    };

    public query func totalSupply() : async Nat {
        var result=0;
        for (value in ledger.vals()){
            result += value;
        };
        return result;
    };

};
