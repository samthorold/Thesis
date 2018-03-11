
module tools

function Sh2(f)
    R = [ mean(f[:,col]) for col = size(f)[2] ]
    R' * inv(cov(f)) * R
end

end
