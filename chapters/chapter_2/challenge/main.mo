import Result "mo:base/Result";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Buffer "mo:base/Buffer";
import Types "types";
actor {

    type Member = Types.Member;
    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;
    type Buffer<X> = Buffer.Buffer<X>;

    let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);

    public shared ({ caller }) func addMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                members.put(caller, member);
                return #ok();
            };
            case (_) {
                return #err("User already added");
            };
        };
    };

    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch (members.get(p)) {
            case (?existingMember) {
                return #ok(existingMember);
            };
            case (_) {
                return #err("User not found");
            };
        };
    };

    public shared ({ caller }) func updateMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (?existingMember) {
                members.put(caller, member);
                return #ok();
            };
            case (_) {
                return #err("User not found");
            };
        };
    };

    public query func getAllMembers() : async [Member] {
        var membersBuff : Buffer<Member> = Buffer.Buffer<Member>(0);
        for (member in members.vals()) {
            membersBuff.add(member);
        };
        return Buffer.toArray(membersBuff);
    };

    public query func numberOfMembers() : async Nat {
        return members.size();
    };

    public shared ({ caller }) func removeMember() : async Result<(), Text> {
        switch (members.get(caller)) {
            case (?existingMember) {
                members.delete(caller);
                return #ok();
            };
            case (_) {
                return #err("User not found");
            };
        };
    };

};
