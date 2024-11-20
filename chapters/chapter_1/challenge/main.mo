import Buffer "mo:base/Buffer";

actor {
    type Buffer<X> = Buffer.Buffer<X>;

    let name : Text = "TalentChain";
    var manifesto : Text = "TalentChain DAO envisions a decentralized future where opportunity is accessible to everyone, regardless of geographical or economic barriers. By leveraging blockchain technology, we aim to redefine how internships and jobs are secured. Our platform empowers individuals to showcase their achievements, connect with companies, and build their careers in the Web3 and ICP ecosystem. This initiative stems from a personal journey: as an engineering student exploring the Web3 space, I discovered the Internet Computer (IC) ecosystem during the Hackerhouse event at Crypto AI Con 2024. Captivated by its potential, I decided to sharpen my skills and embrace this new frontier. In my quest to find an internship and grow within the ICP community, I created TalentChain DAO as a solution not just for myself, but for countless others like me who seek to thrive in this innovative ecosystem. We stand for transparency, equality, and efficiency in talent discovery. Together, we can transform the way talent meets opportunity in the decentralized world.";
    var goals : Buffer<Text> = Buffer.Buffer<Text>(0);

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
        return Buffer.toArray<Text>(goals);
    };
};
