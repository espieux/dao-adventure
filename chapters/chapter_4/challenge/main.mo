import Result "mo:base/Result";
import Buffer "mo:base/Buffer";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Principal "mo:base/Principal";
import Option "mo:base/Option";
import Nat64 "mo:base/Nat64";
import Hash "mo:base/Hash";
import Time "mo:base/Time";
import Bool "mo:base/Bool";
import Types "types";
actor {
    // For this level we need to make use of the code implemented in the previous projects.
    // The voting system will make use of previous data structures and functions.
    /////////////////
    //   TYPES    //
    ///////////////
    type Member = Types.Member;
    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;
    type Proposal = Types.Proposal;
    type ProposalContent = Types.ProposalContent;
    type ProposalId = Types.ProposalId;
    type Vote = Types.Vote;

    /////////////////
    // PROJECT #1 //
    ///////////////
    let goals = Buffer.Buffer<Text>(0);
    let name = "Motoko Bootcamp";
    var manifesto = "Empower the next generation of builders and make the DAO-revolution a reality";

    public shared query func getName() : async Text {
        return name;
    };

    public shared query func getManifesto() : async Text {
        return manifesto;
    };

    public func setManifesto(newManifesto : Text) : async () {
        manifesto := newManifesto;
        return;
    };

    public func addGoal(newGoal : Text) : async () {
        goals.add(newGoal);
        return;
    };

    public shared query func getGoals() : async [Text] {
        Buffer.toArray(goals);
    };

    /////////////////
    // PROJECT #2 //
    ///////////////
    let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);

    public shared ({ caller }) func addMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                members.put(caller, member);
                return #ok();
            };
            case (?member) {
                return #err("Member already exists");
            };
        };
    };

    public shared ({ caller }) func updateMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                members.put(caller, member);
                return #ok();
            };
        };
    };

    public shared ({ caller }) func removeMember() : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                members.delete(caller);
                return #ok();
            };
        };
    };

    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch (members.get(p)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                return #ok(member);
            };
        };
    };

    public query func getAllMembers() : async [Member] {
        return Iter.toArray(members.vals());
    };

    public query func numberOfMembers() : async Nat {
        return members.size();
    };

    /////////////////
    // PROJECT #3 //
    ///////////////
    let ledger = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);

    public query func tokenName() : async Text {
        return "Motoko Bootcamp Token";
    };

    public query func tokenSymbol() : async Text {
        return "MBT";
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance = Option.get(ledger.get(owner), 0);
        ledger.put(owner, balance + amount);
        return #ok();
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance = Option.get(ledger.get(owner), 0);
        if (balance < amount) {
            return #err("Insufficient balance to burn");
        };
        ledger.put(owner, balance - amount);
        return #ok();
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        let balanceFrom = Option.get(ledger.get(from), 0);
        let balanceTo = Option.get(ledger.get(to), 0);
        if (balanceFrom < amount) {
            return #err("Insufficient balance to transfer");
        };
        ledger.put(from, balanceFrom - amount);
        ledger.put(to, balanceTo + amount);
        return #ok();
    };

    public query func balanceOf(owner : Principal) : async Nat {
        return (Option.get(ledger.get(owner), 0));
    };

    public query func totalSupply() : async Nat {
        var total = 0;
        for (balance in ledger.vals()) {
            total += balance;
        };
        return total;
    };
    /////////////////
    // PROJECT #4 //
    ///////////////

    var proposalId : ProposalId = 0;
    let proposals = HashMap.HashMap<ProposalId, Proposal>(0, Nat64.equal, Nat64.toNat32);

    public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
        if(Option.isNull(members.get(caller))){
            return #err("Caller does not exist");
        };
        switch (await burn(caller, 1)) {
            case (#ok()) {
                let result : ProposalId = proposalId;
                var newProposal : Proposal = {
                    id = result;
                    content = content;
                    creator = caller;
                    created = Time.now(); // Use the current time
                    executed = null; // Not executed yet
                    votes = []; // No votes yet
                    voteScore = 0; // Initial score
                    status = #Open; // Initial stat
                };
                proposals.put(result, newProposal);
                proposalId += 1;
                return #ok(result);
            };
            case (#err(msg)) {
                return #err(msg);
            };
        };

    };

    public query func getProposal(proposalId : ProposalId) : async ?Proposal {
        return proposals.get(proposalId);
    };

    public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
        // Check if member exists
        if(Option.isNull(members.get(caller))){
            return #err("Caller does not exist");
        };
        switch(proposals.get(proposalId)){
            // Check if proposal exists
            case(null){
                return #err("Invalid proposal ID");
            };
            case (? proposal){
                // Check if member has already voted
                if (_hasVoted(proposal, caller)){
                    return #err("Already voted");
                };
                // Add vote to proposal
                proposals.put(proposalId, _newProposal(proposal, caller, yesOrNo));
                return #ok();
            };
        };
    };

    public query func getAllProposals() : async [Proposal] {
        return [];
    };

    func _hasVoted(proposal: Proposal, p : Principal) : Bool {
        let iter = proposal.votes.vals();
        for (vote in iter){
            if(vote.member == p){
                return true;
            };
        };
        return false;
    };

    func _newProposal(proposal: Proposal, voter: Principal, yesOrNo : Bool) : Proposal {
        let votingPower = Option.get(ledger.get(voter),0);
        let multiplier = switch(yesOrNo){
            case (true) {1};
            case (false) {-1};
        };
        let newVotes = Buffer.fromArray<Vote>(proposal.votes);
        newVotes.add({
            member = voter;
            votingPower;
            yesOrNo;
        });
        let newVoteScore = proposal.voteScore + votingPower * multiplier;
        var newVoteStatus = proposal.status;
        if (newVoteScore < -100) {
            let newVoteStatus = #Rejected;
        }
        else if (newVoteScore > 100){
            let newVoteStatus = #Accepted;
        };

        let newProposal = {
            id = proposal.id;
            content = proposal.content;
            creator = proposal.creator;
            created = proposal.created;
            executed = proposal.executed;
            votes = Buffer.toArray(newVotes);
            voteScore = newVoteScore;
            status = newVoteStatus;
        };
        return newProposal;
    };
};
