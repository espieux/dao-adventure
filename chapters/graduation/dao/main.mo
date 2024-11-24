import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Nat32 "mo:base/Nat32";
import HashMap "mo:base/HashMap";
import Time "mo:base/Time";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import Types "types";
actor {

        type Result<A, B> = Result.Result<A, B>;
        type Member = Types.Member;
        type ProposalContent = Types.ProposalContent;
        type ProposalId = Types.ProposalId;
        type Proposal = Types.Proposal;
        type Vote = Types.Vote;
        type HttpRequest = Types.HttpRequest;
        type HttpResponse = Types.HttpResponse;

        // The principal of the Webpage canister associated with this DAO canister (needs to be updated with the ID of your Webpage canister)
        stable let canisterIdWebpage : Principal = Principal.fromText("aaaaa-aa");
        stable var manifesto : Text = "TalentChain DAO envisions a decentralized future where opportunity is accessible to everyone, regardless of geographical or economic barriers. By leveraging blockchain technology, we aim to redefine how internships and jobs are secured. Our platform empowers individuals to showcase their achievements, connect with companies, and build their careers in the Web3 and ICP ecosystem. This initiative stems from a personal journey: as an engineering student exploring the Web3 space, I discovered the Internet Computer (IC) ecosystem during the Hackerhouse event at Crypto AI Con 2024. Captivated by its potential, I decided to sharpen my skills and embrace this new frontier. In my quest to find an internship and grow within the ICP community, I created TalentChain DAO as a solution not just for myself, but for countless others like me who seek to thrive in this innovative ecosystem. We stand for transparency, equality, and efficiency in talent discovery. Together, we can transform the way talent meets opportunity in the decentralized world.";
        stable var name : Text = "TalentChain";
        stable var goals : [Text] = [];
        stable var proposalId : ProposalId = 0;
        let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);
         let MBToken = actor ("jaamb-mqaaa-aaaaj-qa3ka-cai") : actor {
                mint(owner : Principal, amount : Nat) : async Result<(), Text>;
                balanceOf(owner : Principal) : async Nat;
                burn(owner : Principal, amount : Nat) : async Result<(), Text>;
        };
        let proposals = HashMap.HashMap<ProposalId, Proposal>(0, Nat.equal, Nat32.fromNat);

        let initialMentor : Member = {
                name = "motoko_bootcamp";
                role = #Mentor;
        };
        let initialMentorPrincipal : Principal = Principal.fromText("nkqop-siaaa-aaaaj-qa3qq-cai");
        members.put(initialMentorPrincipal,initialMentor);

        // Returns the name of the DAO
        public query func getName() : async Text {
                return name;
        };

        // Returns the manifesto of the DAO
        public query func getManifesto() : async Text {
                return manifesto;
        };

        // Returns the goals of the DAO
        public query func getGoals() : async [Text] {
                return goals;
        };

        // Register a new member in the DAO with the given name and principal of the caller
        // Airdrop 10 MBC tokens to the new member
        // New members are always Student
        // Returns an error if the member already exists
        public shared ({ caller }) func registerMember(member : Member) : async Result<(), Text> {
                if (member.role != #Student) return #err("New members are always Student");
                switch (members.get(caller)) {
                        case (null) {
                                members.put(caller, member);
                                return await MBToken.mint(caller, 10);
                        };
                        case (?member) {
                                return #err("Member already registered");
                        };
                };
        };

        // Get the member with the given principal
        // Returns an error if the member does not exist
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

        // Graduate the student with the given principal
        // Returns an error if the student does not exist or is not a student
        // Returns an error if the caller is not a mentor
        public shared ({ caller }) func graduate(student : Principal) : async Result<(), Text> {
                switch (await getMember(caller)) {
                        case (#ok(member)) {
                                if (member.role != #Mentor) return #err("Caller is not a mentor");
                        };
                        case (#err(msg)) {
                                return #err(msg);
                        };
                };
                switch (await getMember(student)) {
                        case (#err(msg)) {
                                return #err(msg);
                        };
                        case (#ok(member)) {
                                if (member.role != #Student) return #err("Graduate must have Student role");
                                let newGraduate : Member = {
                                        name = member.name;
                                        role = #Graduate;
                                };
                                members.put(student, newGraduate);
                                return #ok();
                        };
                };
        };

        // Create a new proposal and returns its id
        // Returns an error if the caller is not a mentor or doesn't own at least 1 MBC token
        public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
                let burnResult = await MBToken.burn(caller, 1);
                if (burnResult != #ok) return #err("Must own at least 1 MBC token to create proposal");
                switch (await getMember(caller)) {
                        case (#ok(member)) {
                                if (member.role != #Mentor) return #err("Caller is not a mentor");
                        };
                        case (#err(msg)) {
                                return #err(msg);
                        };
                };
                let newProposal : Proposal = {
                        id = proposalId; // The unique identifier of the proposal
                        content = content; // The content of the proposal
                        creator = caller; // The member who created the proposal
                        created = Time.now(); // The time the proposal was created
                        executed = null; // The time the proposal was executed or null if not executed
                        votes = []; // The votes on the proposal so far
                        voteScore = 0; // The current score of the proposal based on the votes
                        status = #Open; // The current status of the proposal
                };
                let newProposalId = proposalId;
                proposals.put(newProposalId, newProposal);
                proposalId += 1;
                return #ok(newProposalId);
        };

        // Get the proposal with the given id
        // Returns an error if the proposal does not exist
        public query func getProposal(id : ProposalId) : async Result<Proposal, Text> {
                switch (proposals.get(id)) {
                        case (null) return #err("Proposal does not exist");
                        case (?proposal) return #ok(proposal);
                };
        };

        // Returns all the proposals
        public query func getAllProposal() : async [Proposal] {
                return Iter.toArray(proposals.vals());
        };

        // Vote for the given proposal
        // Returns an error if the proposal does not exist or the member is not allowed to vote
        public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
                // Check if the caller is a member of the DAO
                switch (members.get(caller)) {
                        case (null) return #err("The caller is not a member");
                        case (?member) {
                                // Check if the member has the right to vote
                                if (member.role != #Graduate and member.role != #Mentor) return #err("Only Mentor and Graduate can vote");
                                // Check if the proposal exists
                                switch (proposals.get(proposalId)) {
                                        case (null) return #err("The proposal does not exist");
                                        case (?proposal) {
                                                // Check if the proposal is open for voting
                                                if (proposal.status != #Open) return #err("The proposal is not open for voting");
                                                // Check if the caller has already voted
                                                if (_hasVoted(proposal, caller)) {
                                                        return #err("The caller has already voted on this proposal");
                                                };
                                                let balance : Nat = await MBToken.balanceOf(caller);
                                                var votingPower = balance;
                                                if (member.role == #Mentor) votingPower *= 5;
                                                var multiplierVote = switch (yesOrNo) {
                                                        case (true) { 1 };
                                                        case (false) { -1 };
                                                };
                                                let newVoteScore = proposal.voteScore + votingPower * multiplierVote;
                                                var newExecuted : ?Time.Time = null;
                                                let newVote : Vote = {
                                                        member = caller; // The member who voted
                                                        votingPower = votingPower;
                                                        yesOrNo = yesOrNo; // true = yes, false = no
                                                };
                                                let newVotes = Buffer.fromArray<Vote>(proposal.votes);
                                                newVotes.add(newVote);
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
                                let test = Buffer.fromArray<Text>(goals);
                                test.add(newGoal);
                                goals := Buffer.toArray<Text>(test);
                        };
                        case (#AddMentor(p)) {
                                let _result = _addMentor(p);

                        };
                };
                return;
        };

        func _addMentor(student : Principal) : Result<(), Text> {
                switch (members.get(student)) {
                        case (null) {
                                return #err("student not found");
                        };
                        case (?member) {
                                if (member.role != #Graduate) return #err("New Mentor must have Graduate role");
                                let newGraduate : Member = {
                                        name = member.name;
                                        role = #Graduate;
                                };
                                members.put(student, newGraduate);
                                return #ok();
                        };
                };
        };

        // Returns the Principal ID of the Webpage canister associated with this DAO canister
        public query func getIdWebpage() : async Principal {
                return canisterIdWebpage;
        };

};
