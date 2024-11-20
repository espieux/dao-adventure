import Result "mo:base/Result";
import HashMap "mo:base/HashMap";
import TrieMap "mo:base/TrieMap";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Buffer "mo:base/Buffer";
import Nat64 "mo:base/Nat64";
import Iter "mo:base/Iter";
import Blob "mo:base/Blob";
import Debug "mo:base/Debug";
import Option "mo:base/Option";
import Time "mo:base/Time";
import Array "mo:base/Array";
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
    type DAOStats = Types.DAOStats;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;

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

    func _burn(owner : Principal, amount : Nat) : () {
        let balance = Option.get(ledger.get(owner), 0);
        ledger.put(owner, balance - amount);
        return;
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
    var nextProposalId : Nat64 = 0;
    let proposals = HashMap.HashMap<ProposalId, Proposal>(0, Nat64.equal, Nat64.toNat32);

    public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("The caller is not a member - cannot create a proposal");
            };
            case (?member) {
                let balance = Option.get(ledger.get(caller), 0);
                if (balance < 1) {
                    return #err("The caller does not have enough tokens to create a proposal");
                };
                // Create the proposal and burn the tokens
                let proposal : Proposal = {
                    id = nextProposalId;
                    content;
                    creator = caller;
                    created = Time.now();
                    executed = null;
                    votes = [];
                    voteScore = 0;
                    status = #Open;
                };
                proposals.put(nextProposalId, proposal);
                nextProposalId += 1;
                _burn(caller, 1);
                return #ok(nextProposalId - 1);
            };
        };
    };

    public query func getProposal(proposalId : ProposalId) : async ?Proposal {
        return proposals.get(proposalId);
    };

    public shared ({ caller }) func voteProposal(proposalId : ProposalId, vote : Vote) : async Result<(), Text> {
        // Check if the caller is a member of the DAO
        switch (members.get(caller)) {
            case (null) {
                return #err("The caller is not a member - canno vote one proposal");
            };
            case (?member) {
                // Check if the proposal exists
                switch (proposals.get(proposalId)) {
                    case (null) {
                        return #err("The proposal does not exist");
                    };
                    case (?proposal) {
                        // Check if the proposal is open for voting
                        if (proposal.status != #Open) {
                            return #err("The proposal is not open for voting");
                        };
                        // Check if the caller has already voted
                        if (_hasVoted(proposal, caller)) {
                            return #err("The caller has already voted on this proposal");
                        };
                        let balance = Option.get(ledger.get(caller), 0);
                        let multiplierVote = switch (vote.yesOrNo) {
                            case (true) { 1 };
                            case (false) { -1 };
                        };
                        let newVoteScore = proposal.voteScore + balance * multiplierVote;
                        var newExecuted : ?Time.Time = null;
                        let newVotes = Buffer.fromArray<Vote>(proposal.votes);
                        let newStatus = if (newVoteScore >= 100) {
                            #Accepted;
                        } else if (newVoteScore <= -100) {
                            #Rejected;
                        } else {
                            #Open;
                        };
                        switch (newStatus) {
                            case (#Accepted) {
                                _executeProposal(proposal.content);
                                newExecuted := ?Time.now();
                            };
                            case (_) {};
                        };
                        let newProposal : Proposal = {
                            id = proposal.id;
                            content = proposal.content;
                            creator = proposal.creator;
                            created = proposal.created;
                            executed = newExecuted;
                            votes = Buffer.toArray(newVotes);
                            voteScore = newVoteScore;
                            status = newStatus;
                        };
                        proposals.put(proposal.id, newProposal);
                        return #ok();
                    };
                };
            };
        };
    };

    func _hasVoted(proposal : Proposal, member : Principal) : Bool {
        return Array.find<Vote>(
            proposal.votes,
            func(vote : Vote) {
                return vote.member == member;
            },
        ) != null;
    };

    func _executeProposal(content : ProposalContent) : () {
        switch (content) {
            case (#ChangeManifesto(newManifesto)) {
                manifesto := newManifesto;
            };
            case (#AddGoal(newGoal)) {
                goals.add(newGoal);
            };
        };
        return;
    };

    public query func getAllProposals() : async [Proposal] {
        return Iter.toArray(proposals.vals());
    };

    /////////////////
    // PROJECT #5 //
    ///////////////
    let logo : Text = "<svg version='1.1' id='Layer_1' xmlns:xodm='http://www.corel.com/coreldraw/odm/2003'
        xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' x='0px' y='0px' viewBox='0 0 4000 4000'
        style='enable-background:new 0 0 4000 4000;' xml:space='preserve'>
    <rect style='fill:#FFFFFF;' width='4000' height='4000'/>
    <g>
        <path style='fill:#333333;' d='M1695.72,1726.13c-151.25,0-273.87,122.62-273.87,273.87c0,151.26,122.62,273.87,273.87,273.87
            c151.26,0,273.88-122.61,273.88-273.87C1969.6,1848.75,1846.98,1726.13,1695.72,1726.13z M2302.52,1997.67l224.16-277.31
            l-173.3-300.17l-352.11,55.46c-1.41-0.82-2.81-1.66-4.23-2.47l-128.01-332.62h-346.61l-128.01,332.62
            c-1.42,0.81-2.82,1.65-4.23,2.47l-352.11-55.46l-173.3,300.17l224.16,277.31v4.66l-224.16,277.31l173.3,300.17l352.11-55.46
            c1.41,0.83,2.81,1.66,4.23,2.47l128.01,332.63h346.61l128.02-332.66c1.41-0.81,2.81-1.62,4.21-2.44l352.12,55.46l173.3-300.17
            l-224.17-277.32L2302.52,1997.67z M2292.45,1545.79l95,164.54l-105.91,131.01c-13.45-49.54-33.42-98.19-60.3-144.74
            c-26.88-46.56-59.02-88.18-95.2-124.6L2292.45,1545.79z M1600.73,1256.11h189.99l60.51,157.23
            c-49.63-13.13-101.75-20.15-155.51-20.15c-53.75,0-105.87,7.02-155.5,20.15L1600.73,1256.11z M1099,2454.21l-95-164.54
            l105.9-131.01c13.46,49.55,33.43,98.19,60.31,144.75c26.88,46.55,59.02,88.17,95.2,124.6L1099,2454.21z M1170.21,1696.6
            c-26.88,46.55-46.85,95.2-60.31,144.74L1004,1710.33l95-164.54l166.41,26.21C1229.23,1608.42,1197.09,1650.04,1170.21,1696.6z
            M1790.72,2743.89h-189.99l-60.51-157.22c49.63,13.12,101.75,20.14,155.5,20.14c53.76,0,105.88-7.02,155.51-20.14L1790.72,2743.89z
            M1695.72,2428.01c-236.38,0-428-191.63-428-428.01s191.62-428,428-428s428.01,191.62,428.01,428S1932.1,2428.01,1695.72,2428.01z
            M2387.45,2289.67l-95,164.54l-166.41-26.2c36.18-36.43,68.32-78.05,95.2-124.6c26.88-46.56,46.85-95.2,60.3-144.75
            L2387.45,2289.67z M1695.72,1726.13c-151.25,0-273.87,122.62-273.87,273.87c0,151.26,122.62,273.87,273.87,273.87
            c151.26,0,273.88-122.61,273.88-273.87C1969.6,1848.75,1846.98,1726.13,1695.72,1726.13z'/>
        <path style='fill:#333333;' d='M1969.6,2000c0,151.26-122.62,273.87-273.88,273.87c-151.25,0-273.87-122.61-273.87-273.87
            c0-151.25,122.62-273.87,273.87-273.87C1846.98,1726.13,1969.6,1848.75,1969.6,2000z'/>
        <path style='fill:#333333;' d='M2911.07,1997.67l224.16-277.31l-173.3-300.17l-352.11,55.46c-1.41-0.82-2.81-1.66-4.23-2.47
            l-128.01-332.62h-346.61l-86.67,225.2l7.04,18.29l115.72-18.22l42.22-109.72h189.99l60.51,157.23c-5.76-1.53-11.55-2.97-17.37-4.32
            l169.31,293.25c74.62,77.03,120.56,182.01,120.56,297.73s-45.94,220.7-120.56,297.74l-169.31,293.24
            c5.82-1.35,11.61-2.79,17.37-4.31l-60.51,157.22h-189.99l-42.22-109.71l-122.76,0.07l86.67,225.2h346.61l128.03-332.66
            c1.4-0.81,2.8-1.62,4.2-2.44l352.12,55.46l173.3-300.17l-224.17-277.32L2911.07,1997.67z M2901,1545.79l95,164.54l-105.9,131.01
            c-13.46-49.54-33.43-98.19-60.31-144.74c-26.88-46.56-59.02-88.18-95.2-124.6L2901,1545.79z M2996,2289.67l-95,164.54l-166.41-26.2
            c36.18-36.43,68.32-78.05,95.2-124.6c26.88-46.56,46.85-95.2,60.31-144.75L2996,2289.67z'/>
    </g>
    </svg>";

    func _getWebpage() : Text {
        var webpage = "<style>" #
        "body { text-align: center; font-family: Arial, sans-serif; background-color: #f0f8ff; color: #333; }" #
        "h1 { font-size: 3em; margin-bottom: 10px; }" #
        "hr { margin-top: 20px; margin-bottom: 20px; }" #
        "em { font-style: italic; display: block; margin-bottom: 20px; }" #
        "ul { list-style-type: none; padding: 0; }" #
        "li { margin: 10px 0; }" #
        "li:before { content: '👉 '; }" #
        "svg { max-width: 150px; height: auto; display: block; margin: 20px auto; }" #
        "h2 { text-decoration: underline; }" #
        "</style>";

        webpage := webpage # "<div><h1>" # name # "</h1></div>";
        webpage := webpage # "<em>" # manifesto # "</em>";
        webpage := webpage # "<div>" # logo # "</div>";
        webpage := webpage # "<hr>";
        webpage := webpage # "<h2>Our goals:</h2>";
        webpage := webpage # "<ul>";
        for (goal in goals.vals()) {
            webpage := webpage # "<li>" # goal # "</li>";
        };
        webpage := webpage # "</ul>";
        return webpage;
    };

    func _getMemberNames() : [Text]{
        let memberArray = Iter.toArray(members.vals());
        return Array.map<Member, Text>(memberArray, func(member: Member) {member.name});
    };

    public query func getStats() : async DAOStats {
        return ({
            name;
            manifesto;
            goals = Buffer.toArray(goals);
            members = _getMemberNames();
            logo;
            numberOfMembers = members.size();
        });
    };

    public query func http_request(request :HttpRequest) : async HttpResponse {
        return ({
            status_code = 200;
            headers = [("Content-Type","text/html; charset=UTF-8")];
            body = Text.encodeUtf8(_getWebpage());
            streaming_strategy = null;
        });
    };

};